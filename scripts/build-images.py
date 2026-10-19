#!/usr/bin/env python3
"""Build Linux image archives from an allowlisted, private source snapshot."""

import argparse
import os
from pathlib import Path
import platform
import re
import shutil
import signal
import stat
import subprocess
import sys
import tempfile


ROOT = Path(__file__).resolve().parent.parent
SYSTEMS = ("x86_64-linux", "aarch64-linux")
SOURCE_FILES = (
    "flake.nix", "flake.lock", "jizhang.cabal", "LICENSE", "CHANGELOG.md",
    "client/pubspec.yaml", "client/pubspec.lock",
)
SOURCE_DIRECTORIES = (
    "app", "src", "test", "sql", "ui", "data", "nix", "docker",
    "client/lib", "client/web",
)
SKIPPED_NAMES = frozenset({
    "__pycache__", "build", "dist", "dist-newstyle", "target", "node_modules",
    "cache", "caches", "coverage", "result", "results",
})
ARCHIVE_PATH = re.compile(r"/nix/store/[a-z0-9]{32}-[A-Za-z0-9+._?=-]+\.tar\.gz")


class SnapshotError(Exception):
    """A public input cannot safely be copied."""


def _skip(name):
    return name.startswith(".") or name in SKIPPED_NAMES or name.startswith("result-")


def _open_input(parent_fd, name, relative, *, directory=False):
    flags = os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK
    if directory:
        flags |= os.O_DIRECTORY
    try:
        descriptor = os.open(name, flags, dir_fd=parent_fd)
    except FileNotFoundError:
        raise SnapshotError(f"Missing required input: {relative!r}") from None
    except OSError:
        raise SnapshotError(f"Cannot safely open input: {relative!r}") from None
    metadata = os.fstat(descriptor)
    expected_type = stat.S_ISDIR if directory else stat.S_ISREG
    if not expected_type(metadata.st_mode):
        os.close(descriptor)
        raise SnapshotError(f"Expected {'directory' if directory else 'regular file'}: {relative!r}")
    return descriptor


def _copy_file(parent_fd, name, destination, relative):
    descriptor = _open_input(parent_fd, name, relative)
    with os.fdopen(descriptor, "rb") as source:
        metadata = os.fstat(source.fileno())
        with destination.open("xb") as output:
            os.fchmod(output.fileno(), 0o700 if metadata.st_mode & 0o111 else 0o600)
            shutil.copyfileobj(source, output)


def _copy_directory(descriptor, destination, relative):
    destination.mkdir(mode=0o700)
    for name in sorted(os.listdir(descriptor)):
        if _skip(name):
            continue
        child_relative = f"{relative}/{name}"
        metadata = os.stat(name, dir_fd=descriptor, follow_symlinks=False)
        if stat.S_ISDIR(metadata.st_mode):
            child_fd = _open_input(descriptor, name, child_relative, directory=True)
            try:
                _copy_directory(child_fd, destination / name, child_relative)
            finally:
                os.close(child_fd)
        elif stat.S_ISREG(metadata.st_mode):
            _copy_file(descriptor, name, destination / name, child_relative)
        else:
            raise SnapshotError(f"Symlink or special file is not allowed: {child_relative!r}")


def create_source_snapshot(source_root: Path, destination: Path) -> None:
    """Copy only public build inputs, never following source symlinks.

    The destination must be absent or an empty real directory. The caller owns
    its lifetime; main uses TemporaryDirectory so even partial copies are removed.
    """
    source_root = Path(source_root)
    destination = Path(destination)
    if destination.is_symlink():
        raise SnapshotError("Snapshot destination must be a real directory")
    destination.mkdir(mode=0o700, parents=True, exist_ok=True)
    if any(destination.iterdir()):
        raise SnapshotError("Snapshot destination must be empty")
    destination.chmod(0o700)
    try:
        root_fd = os.open(source_root, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    except OSError:
        raise SnapshotError("Cannot safely open source root") from None
    try:
        for relative in SOURCE_FILES + SOURCE_DIRECTORIES:
            parts = Path(relative).parts
            parent_fd = os.dup(root_fd)
            try:
                for index, name in enumerate(parts[:-1]):
                    next_fd = _open_input(parent_fd, name, "/".join(parts[:index + 1]), directory=True)
                    os.close(parent_fd)
                    parent_fd = next_fd
                target = destination / relative
                target.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
                if relative in SOURCE_DIRECTORIES:
                    child_fd = _open_input(parent_fd, parts[-1], relative, directory=True)
                    try:
                        _copy_directory(child_fd, target, relative)
                    finally:
                        os.close(child_fd)
                else:
                    _copy_file(parent_fd, parts[-1], target, relative)
            finally:
                os.close(parent_fd)
    except OSError:
        # Do not expose absolute workstation paths or file contents in diagnostics.
        raise SnapshotError("Cannot copy public source inputs") from None
    finally:
        os.close(root_fd)


class BuildSession:
    def __init__(self):
        self.process = None
        self.interrupted = None

    def interrupt(self, signum, frame):
        self.interrupted = signum
        self.forward(signum)

    def forward(self, signum):
        if self.process is not None:
            try:
                os.killpg(self.process.pid, signum)
            except ProcessLookupError:
                pass

    def build(self, snapshot, system, target, store):
        with tempfile.NamedTemporaryFile(prefix="jizhang-image-build-", suffix=".log", delete=False) as log:
            log_path = Path(log.name)
            os.fchmod(log.fileno(), 0o600)
            print(f"Building {target}; private diagnostic log: {log_path}", flush=True)
            command = [
                "nix", "build", "--no-write-lock-file", "--no-update-lock-file",
                "--max-jobs", "1", "--cores", "1", "--store", store,
                "--eval-store", store, "--no-link", "--print-out-paths",
                f"path:{snapshot}#packages.{system}.{target}Image",
            ]
            try:
                if self.interrupted is not None:
                    return None
                self.process = subprocess.Popen(
                    command, stdout=subprocess.PIPE, stderr=log,
                    start_new_session=True,
                )
                # A signal received during Popen records cancellation even
                # before the new process group can be addressed.
                if self.interrupted is not None:
                    self.forward(self.interrupted)
                output, _ = self.process.communicate()
                log.write(output)
                if self.interrupted is not None or self.process.returncode:
                    print(f"{target} build {'cancelled' if self.interrupted else 'failed'}; inspect the private log", file=sys.stderr)
                    return None
                archive = output.decode("utf-8", errors="replace").strip()
                if not ARCHIVE_PATH.fullmatch(archive):
                    print(f"{target} returned an unexpected archive path; inspect the private log", file=sys.stderr)
                    return None
            except OSError:
                print(f"Cannot launch or communicate with Nix for {target}; inspect the private log", file=sys.stderr)
                return None
            finally:
                if self.process is not None:
                    if self.process.poll() is None:
                        self.forward(signal.SIGTERM)
                    self.process.wait()
                    self.process = None
        log_path.unlink()
        print(f"{target} archive: {archive}", flush=True)
        return archive


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--system", required=True, choices=SYSTEMS)
    parser.add_argument("--target", choices=("backend", "web", "all"), default="all")
    parser.add_argument("--store", default="auto", metavar="STORE_URI")
    arguments = parser.parse_args()
    if arguments.store == "auto":
        machine = {"x86_64": "x86_64-linux", "aarch64": "aarch64-linux"}.get(platform.machine())
        if platform.system() != "Linux" or machine != arguments.system:
            print("--store auto requires a matching native Linux host; select an explicit Linux build and evaluation store", file=sys.stderr)
            return 1
    session = BuildSession()
    previous_handlers = {}
    try:
        for signum in (signal.SIGINT, signal.SIGTERM):
            previous_handlers[signum] = signal.signal(signum, session.interrupt)
        with tempfile.TemporaryDirectory(prefix="jizhang-image-source-") as temporary:
            snapshot = Path(temporary).resolve() / "source"
            create_source_snapshot(ROOT, snapshot)
            targets = ("backend", "web") if arguments.target == "all" else (arguments.target,)
            for target in targets:
                if session.interrupted is not None:
                    return 128 + session.interrupted
                if session.build(snapshot, arguments.system, target, arguments.store) is None:
                    return 128 + session.interrupted if session.interrupted is not None else 1
        return 0
    except SnapshotError as error:
        print(f"Source snapshot rejected: {error}", file=sys.stderr)
        return 1
    except OSError:
        print("Cannot prepare private image-build files", file=sys.stderr)
        return 1
    finally:
        for signum, handler in previous_handlers.items():
            signal.signal(signum, handler)


if __name__ == "__main__":
    sys.exit(main())

#!/usr/bin/env nix
#! nix develop --no-write-lock-file --impure --expr ``(builtins.getFlake ("git+file://" + toString ../.)).devShells.${builtins.currentSystem}.default`` --command python3
import base64
import binascii
import fcntl
import os
from pathlib import Path
import shlex
import secrets
import signal
import socket
import stat
import subprocess
import sys
import time
import urllib.request

root = Path(__file__).resolve().parent.parent
host = os.environ.get("JIZHANG_HOST", "127.0.0.1")
pg_port = int(os.environ.get("JIZHANG_PG_PORT", "55432"))
frontend_url = f"http://{host}:3000"
backend_url = f"http://{host}:8080"
children = []
pg_root = None
pg_started = False
state_lock = None
http = urllib.request.build_opener(urllib.request.ProxyHandler({}))


def interrupt(signum, frame):
    raise KeyboardInterrupt


signal.signal(signal.SIGTERM, interrupt)
signal.signal(signal.SIGINT, interrupt)


def launch(command, **kwargs):
    process = subprocess.Popen(command, start_new_session=True, **kwargs)
    children.append(process)
    return process


def run(command, *, check=True, **kwargs):
    process = launch(command, **kwargs)
    output, _ = process.communicate()
    result = process.returncode
    children.remove(process)
    if result and check:
        raise subprocess.CalledProcessError(result, command)
    return output if check else result


def wait_ready(url, process):
    deadline = time.monotonic() + 120
    while time.monotonic() < deadline:
        if process.poll() is not None:
            raise RuntimeError(f"Service exited before {url} was ready; inspect the logs")
        try:
            with http.open(url, timeout=2) as response:
                if response.status == 200:
                    return
        except OSError:
            pass
        time.sleep(0.5)
    raise RuntimeError(f"Timed out waiting for {url}; inspect the logs")


def validate_oidc_key(value):
    try:
        decoded = base64.b64decode(value, validate=True)
        if len(decoded) == 32 and base64.b64encode(decoded).decode("ascii") == value:
            return value
    except (ValueError, binascii.Error):
        pass
    raise RuntimeError("OIDC_SECRET_KEY must be canonical standard base64 encoding of exactly 32 bytes")


def secure_state_directory(directory):
    directory.mkdir(parents=True, exist_ok=True, mode=0o700)
    descriptor = os.open(directory, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    try:
        if os.fstat(descriptor).st_uid != os.getuid():
            raise RuntimeError("Local state directory must be owned by the current user")
        os.fchmod(descriptor, 0o700)
    finally:
        os.close(descriptor)


def local_oidc_key(directory):
    # An explicitly supplied value (even empty) must never create or replace a key.
    if "OIDC_SECRET_KEY" in os.environ:
        return validate_oidc_key(os.environ["OIDC_SECRET_KEY"])
    key_path = directory / "oidc-secret.key"
    try:
        descriptor = os.open(key_path, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600)
    except FileExistsError:
        descriptor = os.open(key_path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
        with os.fdopen(descriptor, "rb") as key_file:
            metadata = os.fstat(key_file.fileno())
            if (not stat.S_ISREG(metadata.st_mode) or metadata.st_uid != os.getuid()
                    or stat.S_IMODE(metadata.st_mode) != 0o600 or metadata.st_nlink != 1):
                raise RuntimeError("Existing OIDC key must be an owner-only regular file (mode 0600)")
            encoded = key_file.read(46)
        # Permit only the single terminal newline written by this launcher.
        if encoded.endswith(b"\n"):
            encoded = encoded[:-1]
        try:
            return validate_oidc_key(encoded.decode("ascii"))
        except (UnicodeError, RuntimeError) as error:
            raise RuntimeError("Existing OIDC key is malformed; restore the original key, do not overwrite it") from error
    else:
        with os.fdopen(descriptor, "wb") as key_file:
            os.fchmod(key_file.fileno(), 0o600)
            encoded = base64.b64encode(secrets.token_bytes(32))
            key_file.write(encoded + b"\n")
            key_file.flush()
            os.fsync(key_file.fileno())
        return encoded.decode("ascii")


def main():
    global pg_root, pg_started, state_lock
    if pg_port == 5432 or not 1024 <= pg_port <= 65535:
        raise RuntimeError("JIZHANG_PG_PORT must be 1024–65535, excluding the normal PostgreSQL port 5432")
    if bool(os.environ.get("ADMIN_USERNAME")) != bool(os.environ.get("ADMIN_PASSWORD")):
        raise RuntimeError("Set both ADMIN_USERNAME and ADMIN_PASSWORD, or neither")
    pg_root = root / ".local" / "jizhang"
    secure_state_directory(pg_root)
    state_lock = (pg_root / "launcher.lock").open("a")
    try:
        fcntl.flock(state_lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError as error:
        raise RuntimeError(f"Another launcher is using {pg_root}") from error
    oidc_key = local_oidc_key(pg_root)
    for address, port in [(host, 3000), (host, 8080), ("127.0.0.1", pg_port)]:
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as probe:
                probe.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                probe.bind((address, port))
        except OSError as error:
            raise RuntimeError(f"Cannot bind {address}:{port}: {error}. Check the configured interface and port availability") from error

    run(["cabal", "build", "exe:jizhang"], cwd=root)
    backend_executable = run(["cabal", "list-bin", "exe:jizhang"], cwd=root, stdout=subprocess.PIPE, text=True).strip()
    run(["flutter", "pub", "get"], cwd=root / "client")
    run(["flutter", "build", "web", f"--dart-define=API_BASE_URL={backend_url}"], cwd=root / "client")

    print(f"Persistent testing data and logs: {pg_root}", flush=True)
    data = pg_root / "data"
    if not (data / "PG_VERSION").exists():
        run(["initdb", "-D", str(data), "-U", "postgres", "-A", "trust", "--no-locale", "--encoding=UTF8"], stdout=subprocess.DEVNULL)
    status = run(["pg_ctl", "-D", str(data), "status"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=False)
    if status == 0:
        raise RuntimeError(f"PostgreSQL is already running for {data}; stop that instance before launching")
    pg_started = True
    options = shlex.join(["-h", "127.0.0.1", "-p", str(pg_port), "-k", str(pg_root)])
    run(["pg_ctl", "-D", str(data), "-l", str(pg_root / "postgres.log"), "-o", options, "-w", "start"])
    for database in ["jizhang_local", "jizhang_test"]:
        exists = run(["psql", "-h", "127.0.0.1", "-p", str(pg_port), "-U", "postgres", "-d", "postgres", "-tAc", f"SELECT 1 FROM pg_database WHERE datname = '{database}'"], stdout=subprocess.PIPE, text=True)
        if not exists.strip():
            run(["createdb", "-h", "127.0.0.1", "-p", str(pg_port), "-U", "postgres", database])

    environment = os.environ.copy()
    environment.update(
        BIND_HOST=host,
        DATABASE_URL=f"host=127.0.0.1 port={pg_port} user=postgres dbname=jizhang_local",
        TEST_DATABASE_URL=f"host=127.0.0.1 port={pg_port} user=postgres dbname=jizhang_test",
        CLIENT_ORIGINS=frontend_url,
        PUBLIC_API_URL=backend_url,
        FRONTEND_URL=frontend_url + "/",
        OIDC_SECRET_KEY=oidc_key,
        ADMIN_COOKIE_SECURE="false",
    )
    # Cabal's generated Paths module honors this bundled-data override.
    environment["jizhang_datadir"] = str(root)
    with (pg_root / "backend.log").open("w") as backend_log, (pg_root / "frontend.log").open("w") as frontend_log:
        backend = launch([backend_executable], cwd=root, env=environment, stdout=backend_log, stderr=subprocess.STDOUT)
        wait_ready(backend_url + "/swagger.json", backend)
        frontend = launch(["python3", "-m", "http.server", "3000", "--bind", host, "--directory", str(root / "client/build/web")], stdout=frontend_log, stderr=subprocess.STDOUT)
        wait_ready(frontend_url, frontend)
        print(f"\nReady on the configured interface:\n  Frontend: {frontend_url}\n  Backend:  {backend_url}\n  Admin:    {backend_url}/admin-ui\n\nRegister a user in the frontend. Admin bootstrap is optional via ADMIN_USERNAME/ADMIN_PASSWORD.\nTesting only: HTTP, loopback-only trust PostgreSQL, persistent local data.\nPress Ctrl-C to stop all services; the same database will be reused next run.\n", flush=True)
        while True:
            for process in [backend, frontend]:
                if process.poll() is not None:
                    raise RuntimeError("A service exited unexpectedly; inspect the logs")
            time.sleep(1)


try:
    main()
except KeyboardInterrupt:
    print("\nStopping local stack...", flush=True)
except (OSError, RuntimeError, subprocess.CalledProcessError) as error:
    print(f"Error: {error}", file=sys.stderr)
    sys.exit(1)
finally:
    # Repeated Ctrl-C must not interrupt service/database shutdown.
    signal.signal(signal.SIGINT, signal.SIG_IGN)
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    # Backend and HTTP server are direct children, not Cabal wrapper processes.
    for process in reversed(children):
        try:
            os.killpg(process.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
    for process in reversed(children):
        try:
            process.wait(timeout=10)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.wait()
    if pg_started:
        result = subprocess.run(["pg_ctl", "-D", str(pg_root / "data"), "-m", "fast", "-w", "stop"])
        if result.returncode:
            print(f"PostgreSQL shutdown needs attention: pg_ctl -D {pg_root / 'data'} -m fast -w stop", file=sys.stderr)
    if pg_root:
        print(f"Preserved testing data and logs: {pg_root}", flush=True)
    if state_lock:
        state_lock.close()

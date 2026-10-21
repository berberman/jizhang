{ pkgs, backendPackage, webAssets }:

let
  backendRuntime = pkgs.buildEnv {
    name = "jizhang-backend-runtime";
    paths = [ backendPackage pkgs.curl pkgs.cacert ];
    pathsToLink = [ "/bin" "/share" "/etc" ];
  };
  backendRoot = pkgs.runCommand "jizhang-backend-root" { } ''
    mkdir -p "$out/etc/ssl/certs" "$out/share/licenses/jizhang"
    ln -s ${backendRuntime} "$out/app"
    ln -s ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt \
      "$out/etc/ssl/certs/ca-certificates.crt"
    install -m 0444 ${../LICENSE} "$out/share/licenses/jizhang/LICENSE"
  '';
  webRoot = pkgs.runCommand "jizhang-web-root" { } ''
    mkdir -p "$out/bin" "$out/usr/local/bin" "$out/etc" "$out/share/licenses/jizhang"
    ln -s ${pkgs.busybox}/bin/* "$out/bin/"
    ln -s ${pkgs.static-web-server}/bin/static-web-server "$out/usr/local/bin/static-web-server"
    install -m 0555 ${../docker/start-web.sh} "$out/usr/local/bin/start-web"
    install -m 0444 ${../docker/web.toml} "$out/etc/jizhang-web.toml"
    install -m 0444 ${../LICENSE} "$out/share/licenses/jizhang/LICENSE"
  '';
  imageDefaults = {
    tag = "local";
    created = "1970-01-01T00:00:01Z";
    mtime = "1970-01-01T00:00:01Z";
    includeStorePaths = true;
    includeNixDB = false;
    enableFakechroot = false;
    maxLayers = 64;
  };
  configDefaults = {
    ExposedPorts = { "8080/tcp" = { }; };
    StopSignal = "SIGTERM";
  };
  healthDefaults = {
    Interval = 30000000000;
    Timeout = 6000000000;
    Retries = 3;
  };
in
{
  backendImage = pkgs.dockerTools.buildLayeredImage (imageDefaults // {
    name = "jizhang-backend";
    contents = [ backendRoot ];
    extraCommands = ''
      mkdir -p tmp
      chmod 1777 tmp
    '';
    config = configDefaults // {
      User = "10001:10001";
      Entrypoint = [ "/app/bin/jizhang" ];
      Env = [
        "BIND_HOST=*6"
        "DATABASE_URL="
        "PGCONNECT_TIMEOUT=10"
        "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt"
        "NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt"
      ];
      Healthcheck = healthDefaults // {
        Test = [
          "CMD" "/app/bin/curl" "--fail" "--silent" "--output" "/dev/null"
          "--connect-timeout" "2" "--max-time" "5" "http://127.0.0.1:8080/auth/options"
        ];
        StartPeriod = 60000000000;
      };
    };
  });

  webImage = pkgs.dockerTools.buildLayeredImage (imageDefaults // {
    name = "jizhang-web";
    contents = [ webRoot ];
    extraCommands = ''
      if [ -n "$(find ${webAssets} -type l -print -quit)" ]; then
        echo "Web assets must not contain symlinks" >&2
        exit 1
      fi
      mkdir -p public tmp
      cp -a ${webAssets}/. public/
      chmod -R a-w public
      chmod 1777 tmp
    '';
    fakeRootCommands = ''
      chown 1000:1000 public/runtime
      chmod 0700 public/runtime
    '';
    config = configDefaults // {
      User = "1000:1000";
      Env = [ "PATH=/bin" "API_BASE_URL=same-origin" ];
      Entrypoint = [ "/usr/local/bin/start-web" ];
      Healthcheck = healthDefaults // {
        Test = [
          "CMD-SHELL"
          "wget -q -T 1 -O /dev/null http://127.0.0.1:8080/health && wget -q -T 1 -O /dev/null http://127.0.0.1:8080/ && wget -q -T 1 -O /dev/null http://127.0.0.1:8080/runtime/api-base-url.txt"
        ];
        StartPeriod = 10000000000;
      };
    };
  });
}

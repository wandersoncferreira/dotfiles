with import <nixpkgs> {};

stdenv.mkDerivation rec {
  pname = "clojure";
  version = "1.10.3.814";

  src = fetchurl {
    url = "https://download.clojure.org/install/clojure-tools-${version}.tar.gz";
    sha256 = "fa3a6786e28fbf128903cc43a3d1a244aa4525d3d84494ecb2666d69f69d127e";
  };

  nativeBuildInputs = [
    installShellFiles
    makeWrapper
  ];

  installPhase =
    let
      binPath = lib.makeBinPath [ rlwrap jdk11 ];
    in
      ''
      runHook preInstall
      clojure_lib_dir=$out
      bin_dir=$out/bin
      echo "Installing libs into $clojure_lib_dir"
      install -Dm644 deps.edn "$clojure_lib_dir/deps.edn"
      install -Dm644 example-deps.edn "$clojure_lib_dir/example-deps.edn"
      install -Dm644 exec.jar "$clojure_lib_dir/libexec/exec.jar"
      install -Dm644 clojure-tools-${version}.jar "$clojure_lib_dir/libexec/clojure-tools-${version}.jar"
      echo "Installing clojure and clj into $bin_dir"
      substituteInPlace clojure --replace PREFIX $out
      install -Dm755 clojure "$bin_dir/clojure"
      install -Dm755 clj "$bin_dir/clj"
      wrapProgram $bin_dir/clojure --prefix PATH : $out/bin:${binPath}
      wrapProgram $bin_dir/clj --prefix PATH : $out/bin:${binPath}
      installManPage clj.1 clojure.1
      runHook postInstall
    '';

  doInstallCheck = true;
  installCheckPhase = ''
    CLJ_CONFIG=$out CLJ_CACHE=$out/libexec $out/bin/clojure \
      -Spath \
      -Sverbose \
      -Scp $out/libexec/clojure-tools-${version}.jar
  '';
}

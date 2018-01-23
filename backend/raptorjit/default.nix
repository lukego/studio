# raptorjit nix library API:
#
#   raptorjit.run <luaString>:
#   raptorjit.runFile <path>:
#   raptorjit.runTarball <url>:
#   raptorjit.runDirectory <path>:
#
#     Evaluate Lua code and provide a Studio product.
#     Evaluate a Lua source file. Provide its output and the trace
#     data produced during execution.
#
#   raptorjit.evalString <luaSource>:
#     Helper function to call evalFile with code in string.

{ pkgs ? import ../../nix/pkgs.nix {} }:

with pkgs; with builtins; with stdenv;

rec {
  raptorjit = stdenv.mkDerivation {
    name = "raptorjit-auditlog";
    nativeBuildInputs = [ luajit ];
    src = fetchFromGitHub {
      owner = "lukego";
      repo = "raptorjit";
      rev = "840643881d676459f1c1d77dbd41600b682f8d56";
      sha256 = "0mhs89kxf45jjy6rwkjblq22jjg9phficlmr7l8rfdx70k4mass4";
    };
    installPhase = ''
      install -D src/raptorjit $out/bin/raptorjit
      install -D src/lj_dwarf.dwo $out/lib/raptorjit.dwo
    '';
    enableParallelBuilding = true;  # Do 'make -j'
    dontStrip = true;
  };
  evalTarball = url: evalCode (fetchTarball url);
  evalCode = source:
    runCommand "raptorjit-eval-code"
    {
      src = source;
      nativeBuildInputs = [ raptorjit ];
      dontStrip = true;
    }
    ''
      mkdir $out
      if [ -d $src ]; then
          cp -r $src/* .
      else
          cp $src ./
      fi
      raptorjit -p initial.vmprofile -a audit.log *.lua 2>&1 | tee $out/output.txt
      if [ -f audit.log ]; then
        cp audit.log $out/
      fi
      mkdir -p $out/vmprofile
      find . -name '*.vmprofile' -exec cp {} $out/vmprofile \;
      cp ${raptorjit}/lib/raptorjit.dwo $out/
    '';
  inspect = jit:
    runCommand "inspect" {
        inherit jit;
        dwarfjson = dwarfish.elf2json (toPath "${jit}/raptorjit.dwo");
      }
      ''
        mkdir -p $out/.studio
        cat > $out/.studio/product-info.yaml <<EOF
        type: raptorjit
        EOF
        cp -r $jit/* $out/
        cp $dwarfjson $out/raptorjit-dwarf.json
      '';
  # Run RaptorJIT code and product a Studio product.
  runCode = fileOrDirectory:
    rec {
      raw = evalCode fileOrDirectory;
      product = inspect raw;
    };
  # Convenience wrappers
  run = luaSource: runCode (writeScript "script.lua" luaSource);
  runTarball = url: runCode (fetchTarball url);
  runFile = runCode;
  runDirectory = runCode;
}

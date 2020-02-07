/// Compiler options.

let stringTarget = { "llvc", "node", "web" };

let instructionSet = { "avr", "x64", "x86" };
let environment = { "freestanding", "ubuntu", "uefi", "windows" };

export CompilerOptions = {
  target: stringTarget | instructionSet.fmap(a => environment.map(e => a ++ "-" ++ e))
}

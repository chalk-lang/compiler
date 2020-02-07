/// Main file.

enum Language {
  functionalChalk("chf");
  chalkScript("chs");
  chalkPP("chpp");
  chalkDoc("chdoc");
  
  String abbr;
  String extension;
  
  new(_abbr) : extension('.' ++ abbr) {}
  
  ?Language byAbbr(?String ext) {
    return Language.values.find(lang => lang.extension == abbr);
  }
  
  ?Language byExtension(?String ext) {
    return Language.values.find(lang => lang.extension == ext);
  }
}

class ModulePath {
  pub enum PathType { module, library }
  
  String path;
  
  new(String path) {}
}

pub class Program {
  Promise<String>(String path) loader
  
  Map<ModulePath, Module>
  
  new(_loader) {}
  
  pub Promise loadModule(String path) {
    addModule(parse(await loader(path)));
  }
  
  pub addModule(Module module) {
    // TODO
  }
  
  pub translate(Null(String, OStream) saver, CompilerOptions options) {
    // TODO
  }
  
  pub run(Args) {}
  
  pub Debugger getDebugger(String expression = "Main()") {}
}

class Debugger {
  imt Program program;
  
  cst Resources resources;
  
  // Stores destructed data to enable stepping back.
  Buffer trashBuffer;
  
  int maxTrashSize;
  
  new(_program, _resources) {
    
  }
  
  stepIn();
  
  stepInBack();
  
  stepOver();
  
  stepOverBack();
  
  stepOut();
  
  stepOutBack();
}
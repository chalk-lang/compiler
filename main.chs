/// Main file.

enum Language {
  ///
    TODO Functional Chalk needs a name that is one word (for seo/to avoid confusion).
    
    PureChalk? ChalkImt? Or something else entirely?
  ///
  functionalChalk('Functional Chalk', 'chf');
  chalkScript('ChalkScript', 'chs');
  chalkPP('Chalk++', 'chpp');
  chalkDoc('ChalkDoc', 'chdoc');
  
  String name;
  String abbr;
  String extension;
  
  This(_name, _abbr) : extension('.' ++ abbr) {}
  
  ?Language byAbbr(?String ext) {
    return Language.values.find(lang => lang.extension == abbr);
  }
  
  ?Language byExtension(?String ext) {
    return Language.values.find(lang => lang.extension == ext);
  }
  
  ?Language byName(?String ext) {
    return Language.values.find(lang => lang.extension == ext);
  }
}

class ModulePath {
  pub enum PathType { module, library }
  
  String path;
  
  This(String path) {}
}

export class Program {
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

export class Debugger {
  imt Program program;
  
  let Resources resources;
  
  // Stores destructed data to enable stepping back.
  Buffer trash;
  
  Int64 maxTrashSize;
  
  This(_program, _resources) {
    
  }
  
  step();
  
  stepBack();
  
  stepOver();
  
  stepOverBack();
  
  stepOut();
  
  stepOutBack();
}
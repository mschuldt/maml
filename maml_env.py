class env:
    def __init__(self, parent=None):
        self.names = {}
        self.parent = parent
        self.global_names = set()
        self.types = {}
        
    def name_index(self, name):
        pass
    def name_get_create(self, name):
        pass
    def add_globals(self, names):
        g = self.global_names
        for n in names:
            g.add(n)
        
    def declare(self, decl):
        "DECL is an assign ast node"
        
    def get_type(self, name):
        "returns the type of varaible NAME"
        if name in self.global_names:
            return self.parent.get_type(name)
        typ = self.types.get(name, None)
        if typ: return typ
        #TODO: pass ast node so that line/col numbers can be printed
        print("Error: name '{}' is not declared".format(name))

    def is_declared(self, name):
        "check if NAME is declared"
        if name in self.globals_names:
            return self.parent.is_declared(name)
        return name in self.types

class env:
    def __init__(self, parent=None):
        self.names = {}
        self.parent = parent
        self.global_names = set()
        self.types = {}
        self.n_names = 0
        self.label_counter = -1
        #TODO: track the size of the variable arrays in the Arduino

    def get_store_index(self, name):
        """returns the index at which to store NAME
        return format: (global_p, index)"""
        #check if this variable was declared 'global'
        if name in self.global_names:
            return self.parent.get_store_index(name)

        globalp = (self.parent is None)

        if name in self.names:
            return (globalp, self.names[name])
        else:
            #TODO: check if index is greater then globals variable array on
            #      arduino, send signal/codes to grow it if needed
            #print("declared '{}'".format(name))
            index = self.n_names
            self.names[name] = index
            self.n_names += 1
            return (globalp, index)

    def get_load_index(self, name):
        """returns teh index ast which to load NAME.
        return format: (global_p, index)
        for NAME to have a load index, it must have been stored (declared) first
        (the corresponding call to get_store_index must have been made)"""
        #check if this variable was declared 'global'
        if name in self.global_names:
            return self.parent.get_load_index(name)

        globalp = (self.parent is None)

        if name in self.names:
            return (globalp, self.names[name])

        #else: if we are in local scope, check global scope for index, else error
        if self.parent:
            return self.parent.get_load_index(name)
        #else: no index found
        print("Error: name '{}' is not defined".format(name))
        exit(1) #TODO: just terminate compilation, not the whole program

    def declare_global(self, name):
        assert self.parent, "cannot add globals in global scope"
        self.global_names.add(name)

    def make_label(self):
        "returns a unique label marker"
        self.label_counter += 1
        return self.label_counter

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

class env:

    def __init__(self, parent=None, allow_type_reassign=False):
        self.names = {}
        self.parent = parent
        self.global_names = set()
        self.types = built_in_types
        self.n_names = 0
        self.label_counter = -1
        self.funcTypes = {}
        self.allow_type_reassign = allow_type_reassign
        self.while_start_labels = []
        self.while_end_labels = []
        # TODO: track the size of the variable arrays in the Arduino

    def get_store_index(self, name, ast=None):
        """
        Returns the index at which to store NAME
        return format: (global_p, index)
        """

        # check if this variable was declared 'global'
        if name in self.global_names:
            return self.parent.get_store_index(name, ast)

        globalp = (self.parent is None)

        if name in self.names:
            return (globalp, self.names[name])
        else:
            # TODO: Check if index is greater then globals variable array on
            #       arduino, send signal/codes to grow it if needed
            # print("declared '{}'".format(name))
            index = self.n_names
            self.names[name] = index
            self.n_names += 1
            return (globalp, index)

    def get_load_index(self, name, ast=None):
        """
        Returns the index ast which to load NAME.
        Return format: (global_p, index)
        For NAME to have a load index, it must have been stored (declared)
        first (the corresponding call to get_store_index must have been
        made)
        The type of  AST node is included in error messages if given
        """
        ##?? why do we have two functions for this?
        ##   can we just have a 'get_index' function?

        # Check if this variable was declared 'global'
        if name in self.global_names:
            return self.parent.get_load_index(name)

        globalp = (self.parent is None)

        if name in self.names:
            return (globalp, self.names[name])

        # Else: if we are in local scope, check global scope for index, else
        # error
        if self.parent:
            return self.parent.get_load_index(name)
        # Else: no index found
        print("Error: name '{}' is not defined{}"
              .format(name," (in AST node '{}')".format(ast['type'])))
        exit(1)  # TODO: just terminate compilation, not the whole program

    def declare_global(self, name):
        assert self.parent, "cannot add globals in global scope"
        self.global_names.add(name)

    def make_label(self):
        """
        Returns a unique label marker
        """

        self.label_counter += 1
        return self.label_counter

    def declare_type(self, name, typ):
        """
        Declare NAME to have static type TYP
        """

        type_ = self.types.get(name)
        if type_ and type_ != typ and not self.allow_type_reassign:
            print("Error: cannot re-declare '{}' as type '{}'. was type '{}'"
                  .format(name, typ, type_))
            exit(1)
        self.types[name] = typ

    def get_type(self, name, no_error=False):
        """
        Returns the type of variable NAME
        """

        if self.parent and name in self.global_names:
            return self.parent.get_type(name)
        _type = self.types.get(name, None)
        if _type:
            return _type
        # TODO: pass ast node so that line/col numbers can be printed
        if no_error:
            return None
        else:
            print("Error: (type) name '{}' is not declared".format(name))
            exit(1)

    def is_declared(self, name):
        """
        Check if NAME is declared
        """

        if self.parent and name in self.global_names:
            return self.parent.is_declared(name)
        return name in self.types

    def start_while(self, start_label, end_label):
        self.while_start_labels.append(start_label)
        self.while_end_labels.append(end_label)

    def end_while(self):
        if not self.while_start_labels:
            print("Error: env: ending while loop but label stack is empty")
            exit()
        self.while_start_labels.pop()
        self.while_end_labels.pop()

    def get_while_start_label(self):
        return self.while_start_labels[len(self.while_start_labels)-1]

    def get_while_end_label(self):
        return self.while_end_labels[len(self.while_end_labels)-1]

    def createFuncTypes(self, funcName, argTypes, returnType):
        self._funcTypes[funcName] = self.funcTypes(argTypes, returnType)

    class funcTypes:

        def __init__(self, funcName, argTypes, returnType):
            self.argTypes = argTypes
            self.returnType = returnType

class ftype:
    def __init__(self, a, r):
        self.args = a
        self.ret = r

built_in_types = {'true': 'int',
                  'false': 'int',
                  'none': 'int'}

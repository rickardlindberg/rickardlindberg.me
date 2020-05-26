class Buffer(list):

    def __call__(self, arg):
        self.append(arg)

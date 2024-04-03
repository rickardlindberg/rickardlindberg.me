import doctest

class App:

    @classmethod
    def create(cls):
        """
        >>> isinstance(App.create(), App)
        True
        """
        return cls(
            save_command=SaveCommand.create(),
            share_command=ShareCommand.create(),
        )

    @classmethod
    def create_null(cls):
        return cls(
            save_command=SaveCommand.create_null(),
            share_command=ShareCommand.create_null(),
        )

    def __init__(self, save_command, share_command):
        self.save_command = save_command
        self.share_command  = share_command

    def run(self, args):
        """
        >>> app = App.create_null()
        >>> app.run(["commit"])
        """

class SaveCommand:

    @classmethod
    def create(cls):
        return cls()

    @classmethod
    def create_null(cls):
        return cls()

    def run(self, args):
        """
        >>> SaveCommand.create_null().run([])
        """

class ShareCommand:

    @classmethod
    def create(cls):
        return cls()

    @classmethod
    def create_null(cls):
        return cls()

    def run(self, args):
        """
        >>> ShareCommand.create_null().run([])
        """

if __name__ == "__main__":
    doctest.testmod()
    print("OK")

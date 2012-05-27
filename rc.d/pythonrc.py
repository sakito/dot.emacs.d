# Copyright (C) 2006-2012 sakito <sakito@sakito.com>
import os
HISTORYFILE = "~/var/pythonhistory"
try:
    import readline
except ImportError:
    print("Module readline not available.")
else:
    # history
    histfile = os.path.expanduser(HISTORYFILE)
    try:
        readline.read_history_file(histfile)
    except IOError:
        pass

    try:
        import atexit
        # atexit is available.
        atexit.register(readline.write_history_file, histfile)
        del atexit
    except ImportError:
        # atexit is not available in Python1.5.
        pass

    del histfile

    # tab completion
    try:
        import rlcompleter2
        rlcompleter2.setup()
        del rlcompleter2
    except ImportError:
        import rlcompleter
        # readline and rlcompleter available.
        readline.parse_and_bind("tab: complete")
        #readline.parse_and_bind ("bind ^I rl_complete")
        readline.parse_and_bind("set input-meta on")
        readline.parse_and_bind("set convert-meta off")
        readline.parse_and_bind("set output-meta on")
        del rlcompleter
    del readline

    try:
        import fancycompleter
        fancycompleter.interact()
        del fancycompleter
    except ImportError:
        print("Module fancycompleter not available.")

del os

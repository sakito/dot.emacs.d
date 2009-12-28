(if (eq window-system 'mac)
    (load "~/.emacs.d/site-start.d/init_main.el"))

(if (eq window-system 'ns)
    (load "~/.emacs.d/site-start.d/init_ns.el"))

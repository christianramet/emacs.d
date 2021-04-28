all: init

clean:
	rm -rf custom.el elpa straight/build* var || true

# required until Emacs v26.3 or newer due to an expired gpg key baked into Emacs.
get-keys:
	mkdir -p elpa/gnupg
	chmod 700 elpa/gnupg
	gpg --homedir elpa/gnupg --keyserver keys.gnupg.net --recv-keys 066DAFCB81E42C40

init:
	emacs -Q -l init.el --batch --eval "(let ((confirm-kill-emacs nil) (confirm-kill-processes nil)) (save-buffers-kill-emacs))"

reinstall: clean init

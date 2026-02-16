test:
    emacs --batch -L /home/beat/.config/emacs/.local/straight/build-30.2/dash -l /home/beat/.config/doom/packages/bluesky-test.el -f ert-run-tests-batch-and-exit 2>&1

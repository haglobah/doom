test:
    emacs --batch -L /home/beat/.config/emacs/.local/straight/build-30.2/dash -l /home/beat/.config/doom/packages/bluesky-test.el -f ert-run-tests-batch-and-exit 2>&1

test-difftastic:
    #!/usr/bin/env bash
    b=/home/beat/.config/emacs/.local/straight/build-31.1
    load=$(for p in compat cond-let dash llama magit magit-section transient with-editor difftastic; do printf -- '-L %s/%s ' $b $p; done)
    # Separate runs: groups reorder the buffer the stats tests inspect.
    for t in difftastic-stats-test difftastic-groups-test; do
      emacs --batch $load -l /home/beat/.config/doom/config/$t.el -f ert-run-tests-batch-and-exit 2>&1 || exit 1
    done

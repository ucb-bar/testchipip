CI Notes
----------------

These are the scripts that Circle CI uses to run the tests during a PR.

Note: This uses the Chipyard `ucb-bar/chipyard:dev` branch to run CI (or other similar commits).

Note: CircleCI env. var. refers to adding a environment variable by navigating to the "Project Settings" in the web UI and adding
an environment variable in the "Environment Variables" tab.

Note: This uses `$SERVER` and `$CI_DIR` which is given in the CircleCI env. var. setup to specify a server to build on.
To change these variables you must change the project settings of CircleCI.

Note: You also need to add the private key of the build server to CircleCI and match the fingerprint it gives in the config.yml

# how to push to github using command line
# https://happygitwithr.com/push-pull-github.html#push-pull-github

git config --global user.name 'jhammondusgs'
git config --global user.email 'jhammond@usgs.gov'
git config --global credential.helper osxkeychain
git clone https://github.com/jhammondusgs/powell_streamflow_depletion_regional_analysis
git add 8_development_of_a_gage_classification_scheme.R # to add a file to track

1)

cd powell_streamflow_depletion_regional_analysis

2)

git status

3)

git commit -m "updated code to work with files stored on google drive"


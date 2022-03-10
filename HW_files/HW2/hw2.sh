#!/usr/bin/sh


#### script to set up a github repository to share with another
## plus commands that should help get rid of any early merging errors



## --> 1. Make a new repository via github
### be on desktop





## can also make a repo via github and clone it to the computer
git clone https://github.com/madrharris/SharedRepo
cd SharedRepo/
git pull origin master --allow-unrelated-histories
touch README.md
git add README.md
git commit -m "adding README"
#mkdir notes/
#touch testfile.txt
#mv testfile.txt notes/
#git add notes/testfile.txt
#git commit -m "adding file and directory"
git push



####		gives a few errors?? but works all the same		###

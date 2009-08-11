xgettext R/*.R -kstop -kgettext -o po/R-descr.pot
msgmerge -F -U po/R-pt_BR.po po/R-descr.pot
touch po/R-pt_BR.po

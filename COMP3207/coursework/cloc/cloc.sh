if perl < /dev/null > /dev/null 2>&1  ; then
      echo Found Perl on PATH
else
      echo No Perl found on PATH
fi

perl cloc-1.98.pl --include-lang=Python,JavaScript --list-file=folderlist
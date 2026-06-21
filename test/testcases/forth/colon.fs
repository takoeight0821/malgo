: square dup * ;
: cube dup square * ;
4 cube .
: inc 1 + ;
: triple-inc inc inc inc ;
10 triple-inc .
: sum-of-squares
  dup *
  swap dup *
  + ;
3 4 sum-of-squares .

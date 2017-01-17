% The code herein taken and refactored from this exchange on lilypond-user:
%   http://lilypond.1069038.n5.nabble.com/Is-Anyone-Working-on-a-Better-Tablature-Algorithm-td196422.html

#(lyp:load "lib/chtab.scm")

% ch means Chuanjun He or Christopher Heckman ... take your pick
chTab = #(define-music-function (parser location ly-music) 
  (ly:music?) (chtab:algorithm ly-music))



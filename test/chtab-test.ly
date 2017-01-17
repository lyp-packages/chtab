\require "chtab:.."

vl = { 
  e,16 g\4 cis bes g\4 e'\2 bes e' e'\2 e' bes e'\2 g\4 bes cis g\4 
} 

<< 
  \new Staff { \clef "G_8" \vl } 
  \new TabStaff \vl 
  \chTab { \new TabStaff 
    \with { 
      stringTunings = #guitar-tuning
      instrumentName = "chTab" 
    } \vl }
>> 



mI = { b4 c' d' } 
mII = { g a b } 

ms = << 
  \mI 
  \\ 
  \mII 
>> 

<< 
  \new Staff { \clef "G_8" \ms } 
  \new TabStaff \ms 
  \chTab { \new TabStaff 
    \with { 
      stringTunings = #guitar-tuning 
      instrumentName = "chTab" 
    } \ms }
>> 

% The following music, given as example by Christopher Heckman, currently 
% causes chTab to fail in the same-string-violation-penalty function.

% ns = { a\accent~a4 c~c2 d \glissando f  ~   \glissando <c d> }
% <<
%   \new Staff { \clef "G_8" \ns }
%   \new TabStaff \ns
%   \chTab { \new TabStaff \ns }
% >>
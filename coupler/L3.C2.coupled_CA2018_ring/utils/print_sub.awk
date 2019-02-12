# AWK script used to print out "END SUBROUTINE" at the end of all the SUBROUTINES in
# a module instead of just "END". Who put this in the F90 and standard and why. I wonder?
#
BEGIN{isub=0}
/^  *[Ss][Uu][Bb][Rr][Oo][Uu][Tt][Ii][Nn][Ee]/,/^ *[Ee][Nn][Dd] *$/ {printf("%s",$0); isub=1}
/^ *[Ee][Nn][Dd] *$/{if (isub==1){isub=2}}
{ if      ( isub==0 ) {}
  else if ( isub==1 ) {printf("\n")}
  else if ( isub==2 ) {printf(" SUBROUTINE\n");isub=0;printf(".") | "cat >&2"}
}
END{printf("\n") | "cat >&2"}

# AWK script to print out END FUNCTION at the end of all the FUNCTIONS.
# For some reason F90 standard specifies this, instead of just END and so 
# some compilers choke if its not there - not sure why anyone would put
# this in the standard!
#
BEGIN{isub=0}
/^      .*[F][U][N][C][T][I][O][N]/,/^ *[Ee][Nn][Dd] *$/ {printf("%s",$0); isub=1}
/^ *[Ee][Nn][Dd] *$/{if (isub==1){isub=2}}
{ if      ( isub==0 ) {}
  else if ( isub==1 ) {printf("\n")}
  else if ( isub==2 ) {printf(" FUNCTION\n");isub=0;printf(".") | "cat >&2" }
}
END{printf("\n") | "cat >&2"}

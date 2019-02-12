#!/bin/csh -f
#
# Generate component context switching routines
#
if ( $# == 1 ) then
 set comp_nam = ( $1 )
else
 set comp_nam = ( ocn )
endif
#
cat > component_${comp_nam}_context.c <<EOFA
#include "unistd.h"
#include "dirent.h"
#include "stdio.h"
#include "errno.h"

#define COMP_NAME      "${comp_nam}"
#define DB_LEN         NAME_MAX+1
#define MAX_COMP_COUNT 32

char return_dir[MAX_COMP_COUNT][DB_LEN];
char run_dir[MAX_COMP_COUNT][DB_LEN];

comp_${comp_nam}_push_context_(int *cId)
{ Comp_${comp_nam}_push_context(cId); }
comp_${comp_nam}_push_context__(int *cId)
{ Comp_${comp_nam}_push_context(cId); }
_comp_${comp_nam}_push_context__(int *cId)
{ Comp_${comp_nam}_push_context(cId); }

Comp_${comp_nam}_push_context(int *cId)
{
 int rc;
 if ( *cId > MAX_COMP_COUNT ) {
  printf("Too many components, need to recompiler \"context\" routines with larger value of MAX_COMP_COUNT\n");
  exit(1);
 }
 if ( *cId < 1 ) {
  printf("ERROR: Negative component id passed to \"context\" routines\n");
  exit(1);
 }
 /* Save return directory   */
 getcwd(return_dir[*cId-1], (size_t)DB_LEN);
 snprintf(run_dir[*cId-1],(size_t)DB_LEN,"%s.%d",COMP_NAME,*cId);
 /* Switch to run directory */
 rc=chdir(run_dir[*cId-1]);
 if ( rc != 0 ) {
  fprintf(stderr,"ERROR: Unable to change to component run directory \"%s\", ",run_dir[*cId-1]);
  fprintf(stderr,"\"%s\"\n",strerror(errno));
  exit(1);
 }
}

comp_${comp_nam}_pop_context_(int *cId)
{ Comp_${comp_nam}_pop_context(cId); }
comp_${comp_nam}_pop_context__(int *cId)
{ Comp_${comp_nam}_pop_context(cId); }
_comp_${comp_nam}_pop_context__(int *cId)
{ Comp_${comp_nam}_pop_context(cId); }

Comp_${comp_nam}_pop_context(int *cId)
{
 int rc;

 if ( *cId > MAX_COMP_COUNT ) {
  printf("ERROR: Too many components, need to recompile \"context\" routines with larger value of MAX_COMP_COUNT\n");
  exit(1);
 }
 if ( *cId < 1 ) {
  printf("ERROR: Negative component id passed to \"context\" routines\n");
  exit(1);
 }
 /* Switch to return directory */
 rc=chdir(return_dir[*cId-1]);
 if ( rc != 0 ) {
  fprintf(stderr,"ERROR: Unable to change to component run directory \"%s\", ",run_dir[*cId-1]);
  fprintf(stderr,"\"%s\"\n",strerror(errno));
  exit(1);
 }
}
EOFA

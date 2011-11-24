/* This file is part of descr R package
**
** It is distributed under the GNU General Public License.
** See the file ../LICENSE for details.
** 
** (c) 2009-2011 Jakson Aquino: jalvesaq@gmail.com
**
***************************************************************/

#include <R.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("descr", String)
#else
#define _(String) (String)
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void realfwf2csv(char **fwffile, char **csvfile, char **names, int *begin,
    int *end, int *ncols, int *verbose){

  int i, j, k, len, l = 0, min, max = 0, maxget, nskipped = 0;
  char *b;
  char value[255];
  FILE *fwf, *csv;
  int n = ncols[0];

  /* Convert from R vector to C array */
  for(i = 0; i < n; i++){
    if(end[i] > max)
      max = end[i];
    begin[i] -= 1;
  }
  max += 3;
  min = max - 3;
  maxget = max * 2;

  b = (char*)malloc((maxget + 3) * sizeof(char));
  if(b == NULL){
    REprintf(_("Error: could not allocate memory (%d bytes)\n"), maxget + 3 *
        sizeof(char));
    return;
  }
  fwf = fopen(fwffile[0], "r");
  if(fwf == NULL){
    REprintf(_("Error: could not read file \"%s\".\n"), fwffile[0]);
    return;
  }
  csv = fopen(csvfile[0], "w");
  if(csv == NULL){
    REprintf(_("Error: could not write file \"%s\".\n"), csvfile[0]);
    return;
  }

  /* Put the header in the csv file */
  for(i = 0; i < n; i++){
    if(i < (n - 1))
      fprintf(csv, "%s\t", names[i]);
    else
      fprintf(csv, "%s\n", names[i]);
  }

  /* Put the rows in the csv file */
  while(fgets(b, maxget - 3, fwf)){
    l++;
    len = strlen(b);
    if(len < 3){
      nskipped += 1;
      continue;
    }
    if(len < min){
      REprintf(_("Error: line %d has only %d characters.\n"), l, len);
      fclose(csv);
      fclose(fwf);
      return;
    }
    for(i = 0; i < n; i++){
      j = begin[i];
      k = 0;
      while(j < end[i]){
        value[k] = b[j];
        k++;
        j++;
      }
      value[k] = 0;

      /* delete empty spaces at the end of the field */
      k--;
      while(value[k] == ' ' && k > 0){
        value[k] = 0;
        k--;
      }

      /* put the value into the csv file */
      fprintf(csv, "%s", value);

      /* put either a field separator or the end of line */
      if(i < (n - 1))
        putc('\t', csv);
      else
        putc('\n', csv);
    }
  }

  /* Finish */
  fclose(fwf);
  fclose(csv);
  if(verbose[0] == 1)
      REprintf(_("%d lines written in \"%s\".\n"), l, csvfile[0]);
  if(nskipped == 1)
    REprintf(_("One line from \"%s\" skipped because shorter than 3 characters.\n"), fwffile[0]);
  else
    if(nskipped > 0)
      REprintf(_("%d lines from \"%s\" skipped because shorter than 3 characters.\n"),
          nskipped, fwffile[0]);
}

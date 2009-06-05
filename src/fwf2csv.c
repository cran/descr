#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void realfwf2csv(char **fwffile, char **csvfile, char **names, int *begin,
    int *end, int *ncols){

  int i, j, k, len, l = 0, min, max = 0;
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
  max += 2;
  min = max - 1;

  b = (char*)malloc(max * sizeof(char));
  if(b == NULL)
    return;
  fwf = fopen(fwffile[0], "r");
  if(fwf == NULL)
    return;
  csv = fopen(csvfile[0], "w");
  if(csv == NULL)
    return;

  /* Put the header in the csv file */
  for(i = 0; i < n; i++){
    if(i < (n - 1))
      fprintf(csv, "%s\t", names[i]);
    else
      fprintf(csv, "%s\n", names[i]);
  }

  /* Put the rows in the csv file */
  while(fgets(b, max, fwf)){
    l++;
    for(i = 0; i < n; i++){
      len = strlen(b);
      if(len < min){
	fprintf(csv, "\nError: line %d is too short (%d characters)\n",
            l, len);
        fclose(csv);
        fclose(fwf);
	return;
      }

      j = begin[i];

      /* skip empty spaces at the beginning of the field*/
      while(b[j] == ' ' && j < end[i])
	j++;
      k = 0;
      while(b[j] != ' ' && j < end[i]){
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
}

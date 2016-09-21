#!/bin/bash

echo "REMINDER: this file should be ran inside the log folder"

for FILE in *.csv
do
  LINES=$(cat $FILE | grep -v memory)

  ## Truncate file
  > $FILE

  for LINE in $LINES
  do
    if [ "$LINE" == "Type,Seconds,MegaBytes" ]; then
      echo $LINE >> $FILE
    else
      ## Get unix timestamp
      TIMESTAMP=$(echo $LINE | cut -f2 -d,)

      ## Convert it to readable format
      ## - xargs trims the string
      NEW_TIMESTAMP=$(date --date=@$TIMESTAMP +%r | xargs)

      ## Replace timestamp with readable date
      NEW_LINE=$(echo $LINE | sed "s/[0-9]\+/$NEW_TIMESTAMP/")

      ## Append the new line
      echo $NEW_LINE >> $FILE
    fi
  done
done


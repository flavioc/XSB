#!/bin/csh
##############################################################################
#
# Make the backgrounds of GIFs transparent
#                                                            Simon Moore, 1995
##############################################################################
#  cleargif *.gif

set col = ff/ff/ff

foreach i (*.gif)
    if (-r /tmp/xsbcleargif.gif) then
	\rm /tmp/xsbcleargif.gif
    endif
    echo -n "converting "$i"..."
#   echo "converting colour "$col" to colourless"
    \mv $i /tmp/xsbcleargif.gif
    giftopnm /tmp/xsbcleargif.gif | ppmtogif -transparent rgb:$col > $i
    echo "done"
#      else
#        echo "failed - restoring original"
#        cp /tmp/cleargif.gif $i
#      endif
#    else
#      echo file not found
#    endif
end

echo "--Converted all."

#;;
#0)  echo "Usage: "$0" bgcolour <filenames>"
#    echo "where bgcolour is of the form ff/ff/ff  (white) etc..."
#;;
#esac


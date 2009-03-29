
XSBDIR=$1
 
rm ../config/*/saved.o/*.o ; 

echo "--------------- opt configure -------------------------"

cd ../build

configure > /tmp/config ;
makexsb ;

 echo "--------------- mt-opt configure -------------------------"
 
 configure --enable-mt  > /tmp/config ; 
 makexsb --config-tag=mt ;
 
 echo "--------------- batched configure -------------------------"
 
 configure --enable-batched  > /tmp/config ; 
 makexsb --config-tag=btc ;

#--------------------------------------------------------
 
rm ../config/*/saved.o/*.o ; 

 

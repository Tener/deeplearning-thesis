echo "-----> Finish hook"
rm -f $BUILD_DIR/contents.log
echo "FIXED_HOME:" $FIXED_HOME  >> $BUILD_DIR/contents.log 
find $FIXED_HOME                >> $BUILD_DIR/contents.log 
echo "BUILD_DIR:" $BUILD_DIR    >> $BUILD_DIR/contents.log 
find $BUILD_DIR                 >> $BUILD_DIR/contents.log 
echo "CACHE_DIR:" $CACHE_DIR    >> $BUILD_DIR/contents.log 
find $CACHE_DIR                 >> $BUILD_DIR/contents.log 

if [ ! -z "$INSTALLED_LLVM_VERSION" ]; then
    echo "uninstalling LLVM $INSTALLED_LLVM_VERSION"
    cd "$PATHAUTOPACKOBJ/llvm-$INSTALLED_LLVM_VERSION"
    xargs rm < install_manifest.txt || true
fi
echo "installing LLVM"
cd "$PATHAUTOPACKOBJ/llvm-$VERSIONLLVMPATCH"
run make install

#!/bin/bash

mkdir -p _build
cd _build
cp ../config.h .
cp ../../../webkit/JavaScriptCore/wtf/ASCIICType.h .
patch ASCIICType.h ../ASCIICType.patch
cp ../../../webkit/JavaScriptCore/pcre/* .
./dftables pcre_chartables.h
patch pcre_compile.cpp ../pcre_compile.patch
patch pcre_exec.cpp ../pcre_exec.patch
patch pcre_internal.h ../pcre_internal.patch
patch pcre_tables.cpp ../pcre_tables.patch
patch pcre_ucp_searchfuncs.cpp ../pcre_ucp_searchfuncs.patch
patch pcre.h ../pcre.patch
patch ucpinternal.h ../ucpinternal.patch
mv ucptable.cpp ucptable.h
g++ -c *.cpp

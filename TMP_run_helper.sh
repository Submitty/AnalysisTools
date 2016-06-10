#!/bin/bash


#TMP="csci1100"
TMP="csci2600"
COURSE="/var/local/hss/courses/s16/${TMP}"


#######csci1100
#echo "collect names"
#grep last_name  ${COURSE}/reports/all_grades/*json | awk '{ print $3 }' | sed 's/\"//g' | sed 's/,//g' | sort | uniq > ~/ANON/last_names.txt
#grep first_name ${COURSE}/reports/all_grades/*json | awk '{ print $3 }' | sed 's/\"//g' | sed 's/,//g' | sort | uniq > ~/ANON/first_names.txt
#grep rcs_id     ${COURSE}/reports/all_grades/*json | awk '{ print $3 }' | sed 's/\"//g' | sed 's/,//g' | sort | uniq > ~/ANON/user_names.txt


#######csci2600
echo "collect names"
more  csci2600_classlist.txt  | awk '{ print $3 }' | sort | uniq > ~/ANON/last_names.txt
more  csci2600_classlist.txt  | awk '{ print $4 }' | sort | uniq > ~/ANON/first_names.txt
more  csci2600_classlist.txt  | awk '{ print $2 }' | sort | uniq > ~/ANON/user_names.txt


rm -rf ${TMP}_redacted_first_names
rm -rf ${TMP}_redacted_last_names
rm -rf ${TMP}_redacted_user_names
rm -rf ${TMP}_anonymized_dirs

pushd ~/ANON/GIT_MOSS_PROG_ANALYSIS

make


rm -rf python_course
rm -rf moss_data

cp ../*_names.txt .

echo "remove first names"
cp  -r  ../${TMP}  python_course
./bin/anonymize_dirs python_course -n first_names.txt 
mv moss_data/anonymized/python_course ../${TMP}_redacted_first_names
rm -rf python_course

echo "remove last names"
cp  -r  ../${TMP}_redacted_first_names  python_course
./bin/anonymize_dirs python_course -n last_names.txt 
mv moss_data/anonymized/python_course ../${TMP}_redacted_last_names
rm -rf python_course

echo "remove user names"
cp  -r  ../${TMP}_redacted_last_names  python_course
./bin/anonymize_dirs python_course -n user_names.txt 
mv moss_data/anonymized/python_course ../${TMP}_redacted_user_names
rm -rf python_course


rm -f commands.txt
echo "change directory names"
pwd
ls -lta 
cp  -r  ../${TMP}_redacted_user_names  python_course
ls -lta 
./bin/rename_dirs python_course -n user_names.txt  >  commands.txt
chmod u+x commands.txt
./commands.txt
rm commands.txt
mv python_course ../${TMP}_anonymized_dirs

rm *_names.txt
rm -rf moss_data

popd










echo "done"

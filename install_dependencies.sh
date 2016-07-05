#!/bin/bash

prePWD=$PWD
#################################################################
# REIMINFIR SETUP
#################
cd /bin
wget http://type-inference.googlecode.com/files/type-inference-0.1.2.zip
unzip type-inference-0.1.2.zip
rm type-inference-0.1.2.zip
mv type-inference-0.1.2 type-inference
echo -e "\nexport PATH=\$PATH:/bin/type-inference/binary/\n" >> /etc/environment
chomd 777 /bin/type-inference/binary/javai-reim
sed "s/eval \"java\"/eval \"\/usr\/lib\/jvm\/java-7-openjdk-amd64\/jre\/bin\/java\"/g"  /bin/type-inference/binary/javai-reim > /bin/type-inference/binary/javai-reim
chomd 755 /bin/type-inference/binary/javai-reim
cd $prePWD

#################################################################
# REIMINFIR SETUP
#################

cd lang/java
wget http://www.java2s.com/Code/JarDownload/jdt/jdt-core-1.2.0.jar.zip 
unzip jdt-core-1.2.0.jar.zip
javac -cp jdt-core-1.2.0.jar ./lang/java/*.java


##################################################################
# GRAPH-TOOL INSTALLATION
# used by the class hierarchy analysis tool
############
add-apt-repository "http://downloads.skewed.de/apt/trusty universe"
add-apt-repository ppa:ubuntu-toolchain-r/test

apt-get update
apt-get --yes --force-yes install python-graph-tool




 sudo apt-get update

 sudo apt-get install default-jdk

 java -version

 sudo apt-get install ssh

 sudo apt-get install rsync

 ssh-keygen -t dsa -P ' ' -f ~/.ssh/id_dsa

 cat ~/.ssh/id_dsa.pub >> ~/.ssh/authorized_keys

 wget -c http://mirror.olnevhost.net/pub/apache/hadoop/common/current/hadoop-2.6.0.tar.gz

 sudo tar -zxvf hadoop-2.6.0.tar.gz

 sudo mv hadoop-2.6.0 /usr/local/hadoop

 update-alternatives --config java

 sudo gedit ~/.bashrc

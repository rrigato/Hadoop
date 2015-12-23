 sudo apt-get update

 sudo apt-get install default-jdk

 java -version

 sudo apt-get install ssh

 sudo apt-get install rsync

 ssh-keygen -t dsa -P ' ' -f ~/.ssh/id_dsa

 cat ~/.ssh/id_dsa.pub >> ~/.ssh/authorized_keys

 wget -c http://mirror.olnevhost.net/pub/apache/hadoop/common/current/hadoop-2.6.0.tar.gz
  sudo apt-get install ssh

 sudo apt-get install rsync

 ssh-keygen -t dsa -P ' ' -f ~/.ssh/id_dsa

 cat ~/.ssh/id_dsa.pub >> ~/.ssh/authorized_keys

 wget -c http://mirror.olnevhost.net/pub/apache/hadoop/common/current/hadoop-2.6.0.tar.gz


 sudo tar -zxvf hadoop-2.6.0.tar.gz

 sudo mv hadoop-2.6.0 /usr/local/hadoop

 update-alternatives --config java

 sudo gedit ~/.bashrc


# Login as hadoopuser
 su - hadoopuser
#Generate a ssh key for the user
 ssh-keygen -t rsa -P ""
#Authorize the key to enable password less ssh 
 cat /home/hadoopuser/.ssh/id_rsa.pub >> /home/hadoopuser/.ssh/authorized_keys
 chmod 600 authorized_keys
#Copy this key to slave-1 to enable password less ssh 
 ssh-copy-id -i ~/.ssh/id_rsa.pub slave-1
#Make sure you can do a password less ssh using following command.
 ssh slave-1


 vi /usr/local/hadoop/etc/hadoop/hadoop-env.sh

export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64

export HADOOP_HOME=/home/hadoop/hadoop
export HADOOP_INSTALL=$HADOOP_HOME
export HADOOP_MAPRED_HOME=$HADOOP_HOME
export HADOOP_COMMON_HOME=$HADOOP_HOME
export HADOOP_HDFS_HOME=$HADOOP_HOME
export YARN_HOME=$HADOOP_HOME
export HADOOP_COMMON_LIB_NATIVE_DIR=$HADOOP_HOME/lib/native
export PATH=$PATH:$HADOOP_HOME/sbin:$HADOOP_HOME/bin

export HADOOP_INSTALL=$HADOOP_HOME
export HADOOP_MAPRED_HOME=$HADOOP_HOME
export HADOOP_COMMON_HOME=$HADOOP_HOME
export HADOOP_HDFS_HOME=$HADOOP_HOME
export YARN_HOME=$HADOOP_HOME
export HADOOP_COMMON_LIB_NATIVE_DIR=$HADOOP_HOME/lib/native
export PATH=$PATH:$HADOOP_HOME/sbin:$HADOOP_HOME/bin
 sudo mkdir -p /usr/local/hadoop_store/hdfs/namenode
 sudo mkdir -p /usr/local/hadoop_store/hdfs/datanode
 sudo chown -R hduser:hadoop /usr/local/hadoop_store


 sudo mkdir -p /usr/local/hadoop_store/hdfs/namenode
 sudo mkdir -p /usr/local/hadoop_store/hdfs/datanode
 sudo chown -R hduser:hadoop /usr/local/hadoop_store

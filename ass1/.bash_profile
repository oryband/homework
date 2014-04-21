# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

export M2_HOME=/usr/local/apache-maven/apache-maven-3.2.1/
export M2=$M2_HOME/bin

# JAVA_HOME already defined by os.
#export JAVA_HOME=/usr/lib/jvm/java

PATH=$JAVA_HOME/bin:$M2:$PATH:$HOME/bin
export PATH

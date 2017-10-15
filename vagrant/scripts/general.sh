DEBIAN_FRONTEND=noninteractive

echo 'Installing general utilities...'
apt-get update -y
apt-get install -y curl unzip wget
apt-get install -y build-essential zlib1g-dev
apt-get install -y python-software-properties
systemctl stop cups
systemctl stop cups-browsed
systemctl disable cups
systemctl disable cups-browsed

cat > ~/.wgetrc << EOL
dot_bytes = 5m
EOL

# java
if ! [ -x "$(command -v java)" ]; then
    wget -O /tmp/jdk.tag.gz $1 --header="Cookie: oraclelicense=accept-securebackup-cookie"
    mkdir -p /opt/jdk
    tar -zxf /tmp/jdk.tag.gz -C /opt/jdk --strip-components=1
    update-alternatives --install /usr/bin/java java /opt/jdk/bin/java 100
    update-alternatives --install /usr/bin/javac javac /opt/jdk/bin/javac 100
    
    echo 'export PATH=$PATH:/opt/jdk/bin' >> /etc/profile.d/java.sh
    echo 'export JAVA_HOME=/opt/jdk' >> /etc/profile.d/java.sh
    source /etc/profile.d/java.sh
fi
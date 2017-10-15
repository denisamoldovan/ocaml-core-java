apt-get install -y ocaml

if [ ! -d /opt/eclipse ]; then
    wget -O /tmp/eclipse.tar.gz $1
    mkdir -p /opt/eclipse
    tar xfz /tmp/eclipse.tar.gz -C /opt/eclipse --strip-components=1
    
cat > /usr/share/applications/eclipse.desktop <<EOF
[Desktop Entry]
Version=13.0
Type=Application
Terminal=false
Icon=/opt/eclipse/icon.xpm
Name=Eclipse
Exec=/opt/eclipse/eclipse
EOF
    sudo chmod 644 /usr/share/applications/eclipse.desktop
fi

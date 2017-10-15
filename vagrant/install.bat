if not exist "work" mkdir "work"

vagrant up --provider=virtualbox
vagrant provision
vagrant reload

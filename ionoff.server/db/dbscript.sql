mysqladmin -u root -p'r@@t' password 'kSu<Z7/#ttv2rt!e'
sudo mysql -u root

ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY 'r@@t';

CREATE SCHEMA ionoff DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci ;

CREATE USER 'ionoff'@'%' IDENTIFIED WITH mysql_native_password 'i@n@ff';
CREATE USER 'ionoff'@'%' IDENTIFIED BY 'i@n@ff';

GRANT ALL ON ionoff.* TO 'ionoff'@'%';

FLUSH PRIVILEGES;


mysql -u root -p'r@@t';

ALTER USER 'ionoff'@'%' IDENTIFIED WITH mysql_native_password BY 'i@n@ff';
ALTER USER 'ionoff'@'%' IDENTIFIED BY 'i@n@ff';

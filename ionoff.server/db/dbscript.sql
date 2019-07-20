mysqladmin -u root -p'kSu<Z7/#ttv2rt!e' password 'kSu<Z7/#ttv2rt!e'
sudo mysql -u root

ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY 'kSu<Z7/#ttv2rt!e';

CREATE SCHEMA ionoff DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci ;

CREATE USER 'ionoff'@'localhost' IDENTIFIED BY '7gVwD`qC<@pE![Qp';

CREATE USER 'ionoff'@'localhost' IDENTIFIED BY '?6hqkHk+*D*e+WdY';


GRANT ALL ON ionoff.* TO 'ionoff'@'localhost';
GRANT ALL ON imedia.* TO 'ionoff'@'localhost';

FLUSH PRIVILEGES;


mysql -u root -p'kSu<Z7/#ttv2rt!e';
ALTER USER 'ionoff'@'localhost' IDENTIFIED WITH mysql_native_password BY '?6hqkHk+*D*e+WdY';

ALTER USER 'ionoff'@'localhost' IDENTIFIED BY '?6hqkHk+*D*e+WdY';

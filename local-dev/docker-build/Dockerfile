FROM tomcat:9.0.0.M18-jre8

RUN useradd -U tomcat

COPY target/icenter.war /usr/local/tomcat/webapps/icenter.war

RUN chown -R tomcat /usr/local/tomcat

USER tomcat
EXPOSE 8080
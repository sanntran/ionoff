package net.ionoff.center.server.config;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import org.flywaydb.core.Flyway;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.flyway.FlywayMigrationInitializer;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.context.annotation.Primary;
import org.springframework.orm.hibernate5.HibernateTransactionManager;
import org.springframework.orm.hibernate5.LocalSessionFactoryBean;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.sql.DataSource;
import java.beans.PropertyVetoException;
import java.util.Properties;


@Configuration
@EnableTransactionManagement
public class DatabaseConfig {

    @Autowired
    private ApplicationContext context;

    @Value("${hibernate.connection.driver_class}")
    private String connectionDriverClass;

    @Value("${hibernate.connection.password}")
    private String connectionPassword;

    @Value("${hibernate.connection.url}")
    private String connectionUrl;

    @Value("${hibernate.connection.username}")
    private String connectionUsername;

    @Value("${hibernate.dialect}")
    private String dialect;

    @Autowired
    private DataSource dataSource;

    @Autowired
    private LocalSessionFactoryBean sessionFactoryBean;

    @Bean("sessionFactoryBean")
    @DependsOn({"dataSource"})
    public LocalSessionFactoryBean sessionFactory() {
        LocalSessionFactoryBean sessionFactory = new LocalSessionFactoryBean();
        sessionFactory.setConfigLocation(context.getResource("classpath:hibernate.cfg.xml"));
        sessionFactory.setDataSource(dataSource);
        Properties properties = new Properties();
        properties.put("hibernate.dialect", dialect);
        properties.put("hibernate.connection.driver_class", connectionDriverClass);
        sessionFactory.setHibernateProperties(properties);
        return sessionFactory;
    }

    @Bean("dataSource")
    public ComboPooledDataSource dataSource() {
        ComboPooledDataSource dataSource = new ComboPooledDataSource("jupiter");

        try {
            dataSource.setDriverClass(connectionDriverClass);
        } catch (PropertyVetoException pve){
            System.out.println("Cannot load datasource driver (" + connectionDriverClass +") : " + pve.getMessage());
            return null;
        }
        dataSource.setJdbcUrl(connectionUrl);
        dataSource.setUser(connectionUsername);
        dataSource.setPassword(connectionPassword);
        dataSource.setMinPoolSize(5);
        dataSource.setMaxPoolSize(5);
        dataSource.setMaxIdleTime(30);

        return dataSource;
    }

    @Bean
    @DependsOn({"sessionFactoryBean"})
    public HibernateTransactionManager transactionManager() {
        HibernateTransactionManager transactionManager = new HibernateTransactionManager();
        transactionManager.setSessionFactory(sessionFactoryBean.getObject());
        return transactionManager;
    }
}

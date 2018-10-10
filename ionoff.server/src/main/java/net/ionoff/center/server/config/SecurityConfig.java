package net.ionoff.center.server.config;

import net.ionoff.center.server.persistence.service.IUserService;
import net.ionoff.center.server.security.CorsFilter;
import net.ionoff.center.server.security.JwtAuthenticationTokenFilter;
import net.ionoff.center.server.security.UnauthorizedEntryPoint;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.BeanIds;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.csrf.CsrfFilter;

import javax.annotation.Resource;


@Configuration
@EnableWebSecurity
@ImportResource({"classpath:security-config.xml"})
@EnableGlobalMethodSecurity(prePostEnabled = true)

public class SecurityConfig extends WebSecurityConfigurerAdapter  {

    @Resource(name = "userService")
    private IUserService userService;

    @Autowired
    private UnauthorizedEntryPoint unauthorizedHandler;

    @Autowired
    private CorsFilter corsFilter;

    @Bean(BeanIds.AUTHENTICATION_MANAGER)
    @Qualifier("authenticationManager")
    @Override
    public AuthenticationManager authenticationManagerBean() throws Exception {
        return super.authenticationManagerBean();
    }

    @Autowired
    public void globalUserDetails(AuthenticationManagerBuilder auth) throws Exception {
        auth.userDetailsService(userService).passwordEncoder(encoder());
    }

    @Bean
    public JwtAuthenticationTokenFilter authenticationTokenFilterBean() throws Exception {
        return new JwtAuthenticationTokenFilter();
    }

    @Override
    public void configure(WebSecurity web) throws Exception {
        web.ignoring()
                .antMatchers("/users/license")
                .antMatchers("/users/authenticate")
                .antMatchers("/system/*");
    }

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http
                .addFilterBefore(corsFilter, CsrfFilter.class)
                .csrf().disable()
                .authorizeRequests()
                .anyRequest().authenticated()

                .antMatchers("/projects").hasRole("SYSTEM_ADMIN")
                .antMatchers("/projects/*").hasRole("SYSTEM_ADMIN")
                .antMatchers(HttpMethod.PUT,"/**").hasRole("PROJECT_ADMIN")
                .antMatchers(HttpMethod.DELETE,"/**").hasRole("PROJECT_ADMIN")
                .antMatchers(HttpMethod.PUT,"/playlists").hasRole("PROJECT_USER")
                .antMatchers(HttpMethod.PUT,"/playlists/*").hasRole("PROJECT_USER")
                .antMatchers(HttpMethod.DELETE,"/playlists/*").hasRole("PROJECT_USER")
                .antMatchers(HttpMethod.DELETE,"/playlists/*").hasRole("PROJECT_USER")
                .antMatchers(HttpMethod.DELETE,"/playnodes/*").hasRole("PROJECT_USER")

                .antMatchers(HttpMethod.POST,"/modes/*/activate").hasRole("PROJECT_ADMIN")

                .antMatchers(HttpMethod.POST, "/**/close").hasRole("PROJECT_USER")
                .antMatchers(HttpMethod.POST, "/**/open").hasRole("PROJECT_USER")
                .antMatchers(HttpMethod.POST, "/**/closeopen").hasRole("PROJECT_USER")
                .antMatchers(HttpMethod.POST, "/**/activate").hasRole("PROJECT_USER")
                .antMatchers(HttpMethod.POST, "/**/trigger").hasRole("PROJECT_USER")
                .antMatchers(HttpMethod.POST, "/**/on").hasRole("PROJECT_USER")
                .antMatchers(HttpMethod.POST, "/**/off").hasRole("PROJECT_USER")
                .antMatchers(HttpMethod.POST, "/**/command").hasRole("PROJECT_USER")

                .and()
                .exceptionHandling().authenticationEntryPoint(unauthorizedHandler).and()
                .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS);
        http
                .addFilterBefore(authenticationTokenFilterBean(), UsernamePasswordAuthenticationFilter.class);
    }

    @Bean
    public BCryptPasswordEncoder encoder(){
        return new BCryptPasswordEncoder();
    }
}

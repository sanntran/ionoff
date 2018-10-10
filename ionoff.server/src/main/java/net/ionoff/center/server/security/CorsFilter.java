package net.ionoff.center.server.security;

import org.springframework.stereotype.Component;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;    

@Component
public class CorsFilter implements javax.servlet.Filter {

    @Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        ((HttpServletResponse) response).addHeader("Access-Control-Allow-Origin", "*");
        ((HttpServletResponse) response).addHeader("Access-Control-Allow-Credentials", "true");
        ((HttpServletResponse) response).addHeader("Access-Control-Allow-Methods", "GET, HEAD, OPTIONS, POST, PUT, DELETE");
        ((HttpServletResponse) response).addHeader("Access-Control-Allow-Headers",
        		"access-control-allow-headers,access-control-allow-methods,access-control-allow-origin,x-http-method-override,Origin,X-Requested-With,Content-Type,Accept,Authorization");
        
    	chain.doFilter(request, response);
    }

    @Override
	public void init(FilterConfig fConfig) throws ServletException {
        // does nothing
    }

    @Override
    public void destroy() {
        // does nothing
    }
}
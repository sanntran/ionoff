package net.ionoff.center.server.security;


import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.web.filter.OncePerRequestFilter;

import net.ionoff.center.server.persistence.service.IUserService;

public class JwtAuthenticationTokenFilter extends OncePerRequestFilter {
	
	private final Logger logger = LoggerFactory.getLogger(JwtAuthenticationTokenFilter.class.getName());

    @Autowired
    private JwtTokenUtil jwtTokenUtil;
    
    @Autowired
    private IUserService userService;

    public static final String TOKEN_HEADER = "Authorization";

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain) throws ServletException, IOException {
        String authToken = request.getHeader(TOKEN_HEADER);
        JwtObject jwt = jwtTokenUtil.getJwtObjectFromToken(authToken);
        if (jwt.getUser() != null && SecurityContextHolder.getContext().getAuthentication() == null) {
            // It is not compelling necessary to load the use details from the database.
            UserDetails userDetails = userService.loadUserByUsername(jwt.getUser());
            // For simple validation it is completely sufficient to just check the token integrity.
            if (validateJwtObj(jwt, userDetails)) {
                UsernamePasswordAuthenticationToken authentication = new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());
                authentication.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                SecurityContextHolder.getContext().setAuthentication(authentication);
            }
        }
        chain.doFilter(request, response);
    }
    
    private Boolean validateJwtObj(JwtObject jwt, UserDetails userDetails) {
		if (userDetails == null) {
			return false;
		}
		if (!userDetails.getUsername().equals(jwt.getUser())) {
			return false;
		}
		if (!userDetails.getPassword().equals(jwt.getHash())) {
			return false;
		}
		return !jwt.isExpired();
	}
}
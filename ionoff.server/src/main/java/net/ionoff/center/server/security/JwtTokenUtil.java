package net.ionoff.center.server.security;

import java.io.Serializable;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.springframework.stereotype.Component;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import net.ionoff.center.server.entity.User;

@Component
public class JwtTokenUtil implements Serializable {

	private static final long serialVersionUID = -3301605591108950415L;

	private static final Long ONE_DAY = 86400000L;
	private static final Long ONE_WEEK = ONE_DAY * 7;

	private static final String CLAIM_KEY_USER = "sub";
	private static final String CLAIM_KEY_HASH = "hash";
	private static final String CLAIM_KEY_CREATED = "created";
	private static final String CLAIM_KEY_EXPIRED = "expired";

	private String secret = "4lQ42ohtkG";

	public JwtObject getJwtObjectFromToken(String token) {
		JwtObject jwt = new JwtObject();
		try {
			final Claims claims = getClaimsFromToken(token);
			String user = claims.getSubject();
			jwt.setUser(user);
			String hash = (String) claims.get(CLAIM_KEY_HASH);
			jwt.setHash(hash);
			Date created = new Date((Long) claims.get(CLAIM_KEY_CREATED));
			jwt.setCreated(created);
			Date expiration = claims.getExpiration();
			jwt.setExpiration(expiration);
			Boolean expirated = (Boolean) claims.get(CLAIM_KEY_EXPIRED);
			jwt.setExpired(expirated);
		} catch (Exception e) {
			// Ignore this exception
		}
		return jwt;
	}

	private Claims getClaimsFromToken(String token) {
		Claims claims;
		try {
			claims = Jwts.parser().setSigningKey(secret).parseClaimsJws(token).getBody();
		} catch (Exception e) {
			claims = null;
		}
		return claims;
	}

	public String generateToken(User userDetails, boolean exprired) {
		Map<String, Object> claims = new HashMap<>();
		claims.put(CLAIM_KEY_USER, userDetails.getUsername());
		claims.put(CLAIM_KEY_HASH, userDetails.getPassword());
		final Date createdDate = new Date();
		claims.put(CLAIM_KEY_CREATED, createdDate);
		claims.put(CLAIM_KEY_EXPIRED, exprired);
		Date expirationDate = new Date(createdDate.getTime() + ONE_DAY);
		if (!exprired) {
			expirationDate = new Date(createdDate.getTime() + ONE_WEEK);
		}
		return Jwts.builder().setClaims(claims).setExpiration(expirationDate)
				.signWith(SignatureAlgorithm.HS512, secret).compact();
	}

	public String refreshToken(String token) {
		String newToken;
		try {
			final Claims claims = getClaimsFromToken(token);
			Date now = new Date();
			claims.put(CLAIM_KEY_CREATED, now);
			final Date expirationDate = new Date(now.getTime() + ONE_WEEK);
			newToken = Jwts.builder().setClaims(claims).setExpiration(expirationDate)
					.signWith(SignatureAlgorithm.HS512, secret).compact();
		} catch (Exception e) {
			newToken = null;
		}
		return newToken;
	}
	
}

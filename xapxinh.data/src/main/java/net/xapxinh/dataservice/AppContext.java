package net.xapxinh.dataservice;

import net.xapxinh.dataservice.persistence.service.AlbumSongService;
import net.xapxinh.dataservice.persistence.service.AlbumService;
import net.xapxinh.dataservice.persistence.service.ArtistAlbumService;
import net.xapxinh.dataservice.persistence.service.ArtistSongService;
import net.xapxinh.dataservice.persistence.service.ArtistService;
import net.xapxinh.dataservice.persistence.service.AuthorAlbumService;
import net.xapxinh.dataservice.persistence.service.AuthorService;
import net.xapxinh.dataservice.persistence.service.SongService;
import net.xapxinh.dataservice.persistence.service.PlayerService;
import net.xapxinh.dataservice.search.service.IAlbumSearchService;

import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

public class AppContext implements ApplicationContextAware {

	private static ApplicationContext context;

	public static ApplicationContext getApplicationContext() {
		return context;
	}
	
	public static IAlbumSearchService getAlbumSearchService() {
		return getApplicationContext().getBean("albumSearchService", IAlbumSearchService.class);
	}
	
	public static AlbumService getAlbumService() {
		return getApplicationContext().getBean("albumService", AlbumService.class);
	}
	
	public static AlbumSongService getAlbumSongService() {
		return getApplicationContext().getBean("albumSongService", AlbumSongService.class);
	}
	
	public static AuthorService getAuthorService() {
		return getApplicationContext().getBean("authorService", AuthorService.class);
	}
	
	public static AuthorAlbumService getAuthorAlbumService() {
		return getApplicationContext().getBean("authorAlbumService", AuthorAlbumService.class);
	}
	
	public static ArtistService getArtistService() {
		return getApplicationContext().getBean("artistService", ArtistService.class);
	}
	
	public static ArtistAlbumService getArtistAlbumService() {
		return getApplicationContext().getBean("artistAlbumService", ArtistAlbumService.class);
	}
	
	public static ArtistSongService getArtistSongService() {
		return getApplicationContext().getBean("artistSongService", ArtistSongService.class);
	}
	
	public static SongService getSongService() {
		return getApplicationContext().getBean("songService", SongService.class);
	}
	
	public static PlayerService getPlayerService() {
		return getApplicationContext().getBean("playerService", PlayerService.class);
	}
	
	public void setApplicationContext(ApplicationContext ctx) {
		context = ctx;
	}
}
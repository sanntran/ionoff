package net.xapxinh.dataservice.search.service;

import java.io.IOException;
import java.util.List;

import org.json.JSONException;

import net.xapxinh.dataservice.dto.AlbumDto;

public interface IAlbumSearchService {
	
	/**
	 * @param The key work use to search album
	 * @return List of albums have title, artist name, or author name match the key word.
	 */
	List<AlbumDto> searchAll(String keyword, int page) throws IOException, JSONException;
	
	/**
	 * @param The key work use to search album
	 * @return List of albums have title match the key word
	 */
	List<AlbumDto> searchByTitle(String keyword, int page) throws IOException, JSONException;
	
	/**
	 * @param The key work use to search album
	 * @return List of albums have artist match the key word
	 */
	List<AlbumDto> searchByArtistName(String keyword, int page) throws IOException, JSONException;
	
	/**
	 * @param The key work use to search album
	 * @return List of albums have author match the key word
	 */
	List<AlbumDto> searchByAuthorName(String keyword, int page) throws IOException, JSONException;
}

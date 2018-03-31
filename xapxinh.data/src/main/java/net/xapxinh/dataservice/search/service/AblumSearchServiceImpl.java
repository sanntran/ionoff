package net.xapxinh.dataservice.search.service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import net.xapxinh.dataservice.dto.AlbumDto;
import net.xapxinh.dataservice.entity.Album;
import net.xapxinh.dataservice.search.engine.ISearchEngine;

public class AblumSearchServiceImpl implements IAlbumSearchService {
	
	private static final String CLAZZ = Album.class.getSimpleName();
	private final ISearchEngine searchEngine;
	
	public AblumSearchServiceImpl(ISearchEngine searchEngine) {
		this.searchEngine = searchEngine;
	}
	
	@Override
	public List<AlbumDto> searchAll(String keyword, int page) throws IOException, JSONException {
		List<AlbumDto> albums = new ArrayList<>();
		albums.addAll(searchByTitle(keyword, page));
		albums.addAll(searchByArtistName(keyword, page));
		albums.addAll(searchByAuthorName(keyword, page));
		return albums;
	}

	@Override
	public
	List<AlbumDto> searchByTitle(String keyword, int page) throws JSONException, IOException {
		JSONArray jsonArr = searchEngine.searchByClazz(CLAZZ, "title", keyword, page);
		return toAlbumDtoList(jsonArr);
	}

	@Override
	public
	List<AlbumDto> searchByArtistName(String keyword, int page) throws JSONException, IOException {
		JSONArray jsonArr = searchEngine.searchByClazz(CLAZZ, "artist", keyword, page);
		return toAlbumDtoList(jsonArr);
	}

	@Override
	public
	List<AlbumDto> searchByAuthorName(String keyword, int page) throws JSONException, IOException {
		JSONArray jsonArr = searchEngine.searchByClazz(CLAZZ, "author", keyword, page);
		return toAlbumDtoList(jsonArr);
	}
	
	private List<AlbumDto> toAlbumDtoList(JSONArray jsonArr) throws JSONException {
		if(jsonArr == null) {
			return Collections.emptyList();
		}
		int lengh = jsonArr.length();
		List<AlbumDto> albums = new ArrayList<>(lengh);
		for (int i = 0; i < lengh; i++) {
			JSONObject jsonObj = jsonArr.getJSONObject(i);
			albums.add(toAlbumDto(jsonObj));
		}
		return albums;
	}

	private AlbumDto toAlbumDto(JSONObject jsonObj) throws JSONException {
		AlbumDto album = new AlbumDto();
		album.setId(jsonObj.getLong("id"));
		String name = getString(jsonObj.getJSONArray("name"));
		album.setName(name);
		album.setTitle(getString(jsonObj.getJSONArray("title")));
		if (jsonObj.has("author")) {
			album.setAuthor(getString(jsonObj.getJSONArray("author")));
		}
		if (jsonObj.has("artist")) {
				album.setArtist(getString(jsonObj.getJSONArray("artist")));
		}
		if (jsonObj.has("image")) {
			album.setImage(getString(jsonObj.getJSONArray("image")));
		}
		if (jsonObj.has("release_date")) {
			album.setReleaseDate(getString(jsonObj.getJSONArray("release_date")));
		}
		if (jsonObj.has("listen_count")) {
			String listenCount = getString(jsonObj.getJSONArray("listen_count"));
			try {
				album.setListenCount(Integer.parseInt(listenCount));
			}
			catch (Exception e) {
				// ignore this exception
			}
		}
		return album;
	}

	private String getString(JSONArray jsonArr) throws JSONException {
		if (jsonArr.length() == 0) {
			return "";
		}
		return jsonArr.getString(0);
	}
}

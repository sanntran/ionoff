package net.xapxinh.dataservice.search.engine;

import java.io.IOException;

import org.json.JSONArray;
import org.json.JSONException;

public interface ISearchEngine {
	/**
	 * 
	 * @param clazz Class of object to search
	 * @param field Attribute of the object to search
	 * @param keyword Key word to search
	 * @param page The page of search result. One page has 10 items
	 * @return JSONArray contains list of JSONObject return from search server
	 * @throws IOException If there is an IOException when calling to search-server
	 * @throws JSONException If there is an JSONException when parsing the result from search-server to JSONArray
	 */
	JSONArray searchByClazz(String clazz, String field, String keyword, int page) throws IOException, JSONException;
}

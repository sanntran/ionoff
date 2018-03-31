package net.xapxinh.dataservice.search.engine.solr;

import java.io.IOException;

import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.impl.NoOpResponseParser;
import org.apache.solr.client.solrj.request.QueryRequest;
import org.apache.solr.common.params.CommonParams;
import org.apache.solr.common.util.NamedList;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import net.xapxinh.dataservice.config.DataServiceConfig;
import net.xapxinh.dataservice.search.engine.ISearchEngine;

public class SolrSearchEngine implements ISearchEngine {

	private final String solrUrl;
	private final SolrClient solrClient;
	private static final String WHITE_SPACE = " ";
	private static final String COMMA = ",";
	private static final int PAGE_SIZE = 10;

	public SolrSearchEngine() {
		solrUrl = DataServiceConfig.getInstance().DATASERVICE_SEARCH_URL;
		HttpSolrClient.Builder solrClientBuilder = new HttpSolrClient.Builder();
		solrClientBuilder.withBaseSolrUrl(solrUrl);
		solrClient = solrClientBuilder.build();
	}

	@Override
	public JSONArray searchByClazz(String clazz, String field, String keyword, int page) throws IOException, JSONException {
		if (clazz == null || field == null || keyword == null || keyword.isEmpty()) {
			return null;
		}

		final SolrQuery query = new SolrQuery();
		final String queryString = createSolrQuery(field, keyword);
		final String clazzFilter = createClazzFilter(clazz);

		query.setQuery(queryString);
		query.addFilterQuery(clazzFilter);
		query.set(CommonParams.WT, "json");

		String start = "0";
		if (page > 0) {
			start = ((page - 1) * PAGE_SIZE) + "";
		}
		query.set(CommonParams.START, start);
		query.set(CommonParams.ROWS, PAGE_SIZE + "");
		try {
			final QueryRequest req = new QueryRequest(query);
			req.setResponseParser(new NoOpResponseParser("json"));
			final NamedList<Object> resp = solrClient.request(req);
			final JSONObject jsonObj = new JSONObject((String) resp.get("response"));
			return jsonObj.getJSONObject("response").getJSONArray("docs");
		} catch (final SolrServerException e) {
			throw new IOException("Solr server error:" + e.getMessage());
		}
	}

	private String createClazzFilter(String clazz) {
		return "clazz" + ":" + clazz;
	}

	private String createSolrQuery(String field, String keyword) {
		if (keyword.contains(WHITE_SPACE)) {
			final String[] keys = keyword.split(WHITE_SPACE);
			keyword = StringUtils.join(keys, COMMA);
		}
		return field + ":" + keyword;
	}
}

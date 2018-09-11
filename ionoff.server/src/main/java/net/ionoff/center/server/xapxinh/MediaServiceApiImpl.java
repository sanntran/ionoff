package net.ionoff.center.server.xapxinh;

import net.xapxinh.center.server.api.data.AbstractDataServiceApi;
import org.springframework.beans.factory.annotation.Value;

public class MediaServiceApiImpl extends AbstractDataServiceApi {


	@Value("${service.media.url}")
	private String mediaServiceUrl;


	public MediaServiceApiImpl() {
		super();
	}

	@Override
	public String getDataServiceUrl() {
		return mediaServiceUrl;
	}

}

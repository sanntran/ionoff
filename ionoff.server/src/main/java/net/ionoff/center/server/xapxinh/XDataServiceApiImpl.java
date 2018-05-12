package net.ionoff.center.server.xapxinh;

import net.ionoff.center.server.config.AppConfig;
import net.xapxinh.center.server.api.data.AbstractDataServiceApi;

public class XDataServiceApiImpl extends AbstractDataServiceApi {


	public XDataServiceApiImpl() {
		super();
	}

	@Override
	public String getDataServiceUrl() {
		return AppConfig.getInstance().MEDIA_SERVICE_URL;
	}

}

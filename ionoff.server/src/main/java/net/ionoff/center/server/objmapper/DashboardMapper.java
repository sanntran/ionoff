package net.ionoff.center.server.objmapper;

import net.ionoff.center.server.entity.Dashboard;
import net.ionoff.center.shared.dto.DashboardDto;

public class DashboardMapper implements ObjMapper<Dashboard, DashboardDto> {
	
	@Override
	public DashboardDto createDto(Dashboard dashboard) {
		final DashboardDto dashboardDto = new DashboardDto();
		return dashboardDto;
	}

}

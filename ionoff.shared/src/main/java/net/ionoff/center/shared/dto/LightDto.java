package net.ionoff.center.shared.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName( "LightDto" ) 
public class LightDto extends DeviceDto {
	
	private static final long serialVersionUID = 1L;
	
	public String getClazz() {
		return "LightDto";
	}
	
	@Override
	public String getStyle() {
		return "light";
	}
}

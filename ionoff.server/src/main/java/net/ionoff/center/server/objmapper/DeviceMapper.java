package net.ionoff.center.server.objmapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.Appliance;
import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.EntityUtil;
import net.ionoff.center.server.entity.Light;
import net.ionoff.center.server.entity.Player;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.WeighScale;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.IZoneService;
import net.ionoff.center.shared.dto.ApplianceDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.LightDto;
import net.ionoff.center.shared.dto.PlayerDto;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.WeighScaleDto;

public class DeviceMapper {
	
	@Autowired
	private RelayMapper relayMappper;
	@Autowired
	private IZoneService zoneService;
	@Autowired
	private IDeviceService deviceService;


	public Device createDevice(DeviceDto deviceDto) {
		final Device device = createNewDevice(deviceDto);
		updateDevice(device, deviceDto);
		final Zone zone = zoneService.findById(deviceDto.getZoneId());
		device.setZone(zone);
		device.setProject(zone.getProject());
		return device;
	}

	public Device updateDevice(Device device, DeviceDto deviceDto) {
		if (device instanceof Player) {
			final PlayerDto playerDto = (PlayerDto)deviceDto;
			Player player = (Player) device;
			player.setMac(playerDto.getMac());				
			player.setModel(playerDto.getModel());
		}
		if (device instanceof WeighScale) {
			final WeighScaleDto scaleDto = (WeighScaleDto)deviceDto;
			WeighScale scale = (WeighScale) device;
			scale.setMac(scaleDto.getMac());				
			scale.setModel(scaleDto.getModel());
		}
		device.setName(deviceDto.getName());
		device.setOrder(deviceDto.getOrder());
		return device;
	}

	private Device createNewDevice(DeviceDto deviceDto) {
		if (deviceDto instanceof PlayerDto) {
			final Player player = new Player();
			return player;
		}
		if (deviceDto instanceof WeighScaleDto) {
			final WeighScale scale = new WeighScale();
			return scale;
		}
		else if (deviceDto instanceof LightDto) {
			return new Light();
		}
		return new Appliance();
	}
	
	public List<DeviceDto> toDeviceDtoList(List<? extends Device> devices) {
		final List<DeviceDto> deviceDtos = new ArrayList<DeviceDto>();
		for (final Device device : devices) {
			deviceDtos.add(createDeviceDto(device));
		}
		return deviceDtos;
	}
	
	public DeviceDto createDeviceDto(Device device) {
		final DeviceDto deviceDto = newDeviceDto(EntityUtil.castUnproxy(device, Device.class));
		deviceDto.setStatus(deviceService.getStatusDto(device));
		deviceDto.setId(device.getId());
		deviceDto.setName(device.getName());
		deviceDto.setOrder(device.getOrder());
		deviceDto.setZoneId(device.getZone().getId());
		deviceDto.setZoneName(device.getZone().getName());
		deviceDto.setAreaName(device.getZone().getArea().getName());
		deviceDto.setProjectId(device.getProject().getId());
		return deviceDto;
	}

	private DeviceDto newDeviceDto(Device device) {
		if (device instanceof Player) {
			return createPlayerDto(device);
		}
		if (device instanceof WeighScale) {
			return createWeighScaleDto(device);
		}
		if (device instanceof Light) {
			return new LightDto();
		}
		return createApplianceDto(device);
	}

	private ApplianceDto createApplianceDto(Device device) {
		final ApplianceDto applianceDto = new ApplianceDto();
		final List<RelayDto> relayDtos = new ArrayList<RelayDto>();
		for (final Relay relay : device.getRelayList()) {
			relayDtos.add(relayMappper.createRelayDto(relay));
		}
		applianceDto.setRelays(relayDtos);
		return applianceDto;
	}
	
	private DeviceDto createPlayerDto(Device device) {
		final PlayerDto playerDto = new PlayerDto();
		final Player player = (Player) device;
		playerDto.setMac(player.getMac());
		playerDto.setIp(player.getIp());
		playerDto.setModel(player.getModel());
		return playerDto;
	}

	private DeviceDto createWeighScaleDto(Device device) {
		final WeighScaleDto scaleDto = new WeighScaleDto();
		final WeighScale scale = (WeighScale) device;
		scaleDto.setMac(scale.getMac());
		scaleDto.setModel(scale.getModel());
		return scaleDto;
	}

}


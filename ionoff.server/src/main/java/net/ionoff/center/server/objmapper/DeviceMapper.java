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
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorDriver;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.ApplianceDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.LightDto;
import net.ionoff.center.shared.dto.PlayerDto;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.SensorDriverDto;
import net.xapxinh.center.server.service.player.IPlayerService;
import net.xapxinh.center.shared.dto.StatusDto;

public class DeviceMapper {
	
	@Autowired
	private RelayMapper relayMappper;
	
	public Device createDevice(DeviceDto deviceDto, Zone zone) {
		final Device device = createNewDevice(deviceDto);
		updateDevice(device, deviceDto);
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
		if (device instanceof SensorDriver) {
			final SensorDriverDto sensorDriverDto = (SensorDriverDto)deviceDto;
			SensorDriver sensorDriver = (SensorDriver) device;
			sensorDriver.setMac(sensorDriverDto.getMac());				
			sensorDriver.setModel(sensorDriverDto.getModel());
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
		if (deviceDto instanceof SensorDriverDto) {
			final SensorDriver sensorDriver = new SensorDriver();
			return sensorDriver;
		}
		else if (deviceDto instanceof LightDto) {
			return new Light();
		}
		return new Appliance();
	}
	
	public List<DeviceDto> toDeviceDtoList(List<? extends Device> devices , IPlayerService playerService) {
		final List<DeviceDto> deviceDtos = new ArrayList<DeviceDto>();
		for (final Device device : devices) {
			deviceDtos.add(createDeviceDto(device, playerService));
		}
		return deviceDtos;
	}
	
	public DeviceDto createDeviceDto(Device device, IPlayerService playerService) {
		final DeviceDto deviceDto = newDeviceDto(EntityUtil.castUnproxy(device, Device.class));
		deviceDto.setStatus(getStatusDto(device, playerService));
		deviceDto.setId(device.getId());
		deviceDto.setName(device.getName());
		deviceDto.setOrder(device.getOrder());
		deviceDto.setZoneId(device.getZone().getId());
		deviceDto.setZoneName(device.getZone().getName());
		deviceDto.setAreaName(device.getZone().getArea().getName());
		deviceDto.setProjectId(device.getProject().getId());
		return deviceDto;
	}

	public List<net.ionoff.center.shared.dto.StatusDto> toStatusDto(List<Device> devices, IPlayerService playerService) {
		List<net.ionoff.center.shared.dto.StatusDto> statusDtos = new ArrayList<>();
		for (Device device : devices) {
			statusDtos.add(getStatusDto(device, playerService));
		}
		return statusDtos;
	}
	
	public net.ionoff.center.shared.dto.StatusDto getStatusDto(Device device, IPlayerService playerService) {
		net.ionoff.center.shared.dto.StatusDto statusDto = new net.ionoff.center.shared.dto.StatusDto();
		statusDto.setId(device.getId());
		statusDto.setValue(device.getStatus());
		if (device.getTime() != null) {
			statusDto.setTime(DateTimeUtil.ddMMHHmmFormatter.format(device.getTime()));
		}
		if (device.instanceOf(Appliance.class)) {
			for (Relay relay : device.getRelayList()) {
				net.ionoff.center.shared.dto.StatusDto child = new net.ionoff.center.shared.dto.StatusDto();
				child.setId(relay.getId());
				child.setValue(relay.getStatus());
				if (relay.getTime() != null) {
					child.setTime(DateTimeUtil.ddMMHHmmFormatter.format(relay.getTime()));
				}
				statusDto.getChildren().add(child);
			}
		}
		else if (device.instanceOf(SensorDriver.class)) {
			SensorDriver sensorDriver = EntityUtil.castUnproxy(device, SensorDriver.class);

			if (sensorDriver.getSensors() != null && !sensorDriver.getSensors().isEmpty()) {
				Sensor sensor = sensorDriver.getSensors().get(0);
				if (sensor.getStatus().getTime() != null) {
					statusDto.setTime(DateTimeUtil.ddMMHHmmFormatter.format(sensor.getStatus().getTime()));
				}
				statusDto.setLatestValue(sensor.getStatus().getValue() + " " + sensor.getUnit());
			}
		}
		else if (device.instanceOf(Player.class) && playerService != null) {
			try {
				Player player = EntityUtil.castUnproxy(device, Player.class);
				StatusDto status = playerService.requesStatus(toPlayer(player), null);
				statusDto.setState(status.getState());
				statusDto.setTrack(status.getTitle());
				if (status.getPosition() > 0) {
					statusDto.setPosition(Math.round(status.getPosition() * 100));
				}
			} catch (Exception e) {
				//
			}
		}
		return statusDto;
	}

	public net.xapxinh.center.server.entity.Player toPlayer(Device device) {
		Player player = (net.ionoff.center.server.entity.Player) EntityUtil.castUnproxy(device, Device.class);
		net.xapxinh.center.server.entity.Player p = new net.xapxinh.center.server.entity.Player();
		p.setId(player.getId());
		p.setName(player.getName());
		p.setMac(player.getMac());
		p.setIp(player.getIp());
		p.setPort(player.getPort());
		p.setModel(player.getModel());
		return p;
	}

	public DeviceDto newDeviceDto(Device device) {
		if (device instanceof Player) {
			return createPlayerDto(device);
		}
		if (device instanceof SensorDriver) {
			return createSensorDriverDto(device);
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
	
	private static DeviceDto createPlayerDto(Device device) {
		final PlayerDto playerDto = new PlayerDto();
		final Player player = (Player) device;
		playerDto.setMac(player.getMac());
		playerDto.setIp(player.getIp());
		playerDto.setModel(player.getModel());
		return playerDto;
	}

	private static DeviceDto createSensorDriverDto(Device device) {
		final SensorDriverDto sensorDriverDto = new SensorDriverDto();
		final SensorDriver sensorDriver = (SensorDriver) device;
		sensorDriverDto.setMac(sensorDriver.getMac());
		sensorDriverDto.setModel(sensorDriver.getModel());
		return sensorDriverDto;
	}

	public DeviceDto createDeviceDto(Device device) {
		return createDeviceDto(device, null);
	}
}


package net.ionoff.things.e4;

import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.MatteBorder;

import net.ionoff.things.ConfigTool;
import net.ionoff.things.TcpServer;

public class E4TestPanel extends JPanel {
	
	private static final long serialVersionUID = 1L;

	private ConfigTool e4Tool;
	
	private List<JLabel> statusLbls;
	private List<JButton> onBtns;
	private List<JButton> offBtns;
	private List<JButton> onOffBtns;
	
	
	private MatteBorder grayBorder = BorderFactory.createMatteBorder(5, 3, 5, 3, Color.LIGHT_GRAY);
	private MatteBorder blackBorder = BorderFactory.createMatteBorder(5, 3, 5, 3, Color.BLACK);
	private MatteBorder greenBorder = BorderFactory.createMatteBorder(5, 3, 5, 3, Color.GREEN);
	
	public E4TestPanel(ConfigTool p8Tool) {
		
		this.e4Tool = p8Tool;
		
		setLayout(null);
		JPanel container = new JPanel();
		container.setBounds(10, 25, 510, 140);
		add(container);
		
		container.setLayout(new GridLayout(4, 4, 8, 8));
		
		statusLbls = new ArrayList<>(4);
		onBtns = new ArrayList<>(4);
		offBtns = new ArrayList<>(4);
		onOffBtns = new ArrayList<>(4);
		
		for (int i = 0; i < 4; i++) {
			final int relayIndex = i + 1;
			
			JLabel relayName = new JLabel("         Relay " + (i + 1));
			relayName.setBackground(Color.WHITE);
			relayName.setSize(100, 25);
			relayName.setBorder(grayBorder);
			container.add(relayName);
			statusLbls.add(relayName);
		}
		
		
		for (int i = 0; i < 4; i++) {
			final int relayIndex = i + 1;
			JButton onBtn = new JButton("On");	
			onBtn.setSize(100, 26);
			container.add(onBtn);
			onBtns.add(onBtn);
			
			
			onBtn.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					setControlStatus(relayIndex, 1);
				}
			});
			
		}
		
		for (int i = 0; i < 4; i++) {
			final int relayIndex = i + 1;
		
			
			JButton offBtn = new JButton("Off");	
			offBtn.setSize(100, 26);
			container.add(offBtn);
			offBtns.add(offBtn);
		
			
			offBtn.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					setControlStatus(relayIndex, 0);
				}
			});
			
		}
		

		for (int i = 0; i < 4; i++) {
			final int relayIndex = i + 1;
	
			
			JButton onOffBtn = new JButton("On-Off");	
			onOffBtn.setSize(100, 26);
			container.add(onOffBtn);
			onOffBtns.add(onOffBtn);
			
			
			onOffBtn.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					setControlStatus(relayIndex, 2);
				}
			});
		}
	}

	public void setControlStatus(int relayIndex, int controlCode) {
		if (!e4Tool.validateKey()) {
			e4Tool.showMessage("Key is invalid", Color.RED);
			return;
		}
		try {
			E4Status status = TcpServer.getInstance().sendReqSetStatus(e4Tool.getKeyTextField().getText(), relayIndex, controlCode);
			setStatus(status);
		} catch (Exception e) {
			e.printStackTrace();
			e4Tool.showMessage(e.getMessage(), Color.RED);
		}
	}

	public void setStatus(E4Status status) {
		if (status == null) {
			for (int i = 0; i < 4; i++) {
				statusLbls.get(i).setBorder(grayBorder);
			}
			return;
		}
		for (int i = 0; i < 4; i++) {
			if (status.getOutputStates().get(i) == null) {
				statusLbls.get(i).setBorder(grayBorder);
			}
			else if (status.getOutputStates().get(i) == true) {
				statusLbls.get(i).setBorder(greenBorder);
			}
			else if (status.getOutputStates().get(i) == false) {
				statusLbls.get(i).setBorder(blackBorder);
			}
		}
	}
	
	public void setConnected() {
		for (int i = 0; i < 4; i++) {
			onBtns.get(i).setEnabled(true);
			offBtns.get(i).setEnabled(true);
			onOffBtns.get(i).setEnabled(true);
		}
	}
	public void setDisConnected() {
		setStatus(null);
		for (int i = 0; i < 4; i++) {
			onBtns.get(i).setEnabled(false);
			offBtns.get(i).setEnabled(false);
			onOffBtns.get(i).setEnabled(false);
		}
	}
}

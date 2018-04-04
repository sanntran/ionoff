package net.ionoff.things.p8;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import com.alee.laf.WebLookAndFeel;

public class P8Tool extends JFrame {

	private static final long serialVersionUID = 1L;
	static final boolean PUBLIC_MODE = false; 
	
	private JTextField ipTextField;
	private JTextField pwTextField;
	
	private JLabel messageLbl;
	private JTabbedPane tabbedPane;
	private ConfigPanel confPanel;
	private ControlPanel controlPanel;

	// Create a form with the fields
	public P8Tool() {

		super("IOnOff P8 Tool");

		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setSize(370, 450);
		setResizable(false);

		JMenuBar menuBar = new JMenuBar();

		JMenu fileMenu = new JMenu("File");
		menuBar.add(fileMenu);
		JMenuItem quitMenuItem = new JMenuItem("Quit");
		quitMenuItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				System.exit(0);
			}
		});
		fileMenu.add(quitMenuItem);
		setJMenuBar(menuBar);

		JPanel contentPane = new JPanel();
		contentPane.setLayout(null);
		this.setContentPane(contentPane);
		
		
		////////////////--------------------------------------------------------------------
		JPanel ipPassPanel = new JPanel();
		ipPassPanel.setBounds(0, 0, 365, 55);
		ipPassPanel.setBackground(new Color(217, 217, 217));
		ipPassPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY));
		ipPassPanel.setLayout(null);
		JLabel ipLbl = new JLabel("IP");
		ipLbl.setBounds(10, 15, 35, 25);
		ipPassPanel.add(ipLbl);
		ipTextField = new JTextField(20);
		ipTextField.setEnabled(false);
		ipTextField.setBounds(40, 15, 150, 25);
		ipPassPanel.add(ipTextField);
		
		JLabel keyLbl = new JLabel("Key");
		keyLbl.setBounds(205, 15, 25, 25);
		ipPassPanel.add(keyLbl);
		pwTextField = new JTextField(20);
		pwTextField.setEnabled(false);
		pwTextField.setText("");
		pwTextField.setBounds(235, 15, 120, 25);
		ipPassPanel.add(pwTextField);
		
		contentPane.add(ipPassPanel);
		
		////////////////--------------------------------------------------------------------
		JPanel messagePanel = new JPanel();
		messagePanel.setBackground(new Color(217, 217, 217));
		messagePanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY));
		messagePanel.setBounds(0, 55, 365, 25);
		messagePanel.setLayout(null);
		messageLbl = new JLabel("");
		messageLbl.setHorizontalAlignment(SwingConstants.CENTER);
		messageLbl.setBounds(10, 0, 345, 25);
		messagePanel.add(messageLbl);
		contentPane.add(messagePanel);
		
		////////////////--------------------------------------------------------------------
		confPanel = new ConfigPanel(this);
		controlPanel = new ControlPanel(this);
		////////////////--------------------------------------------------------------------
		tabbedPane = new JTabbedPane();
		tabbedPane.setBounds(0, 80, 365, 315);
		
		tabbedPane.addTab("Config", confPanel);
		tabbedPane.addTab("Control", controlPanel);
		
		contentPane.add(tabbedPane);
		
		
		////////////////--------------------------------------------------------------------
		setLocationRelativeTo(null);
		this.setVisible(true);
	}
	
	void showMessage(final String string, final Color color) {
		messageLbl.setText(string);
		messageLbl.setForeground(color);
		messageLbl.repaint();
	}

	public static void main(String[] args) {
		WebLookAndFeel.install();
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				P8Tool p8Tool = new P8Tool();
				TcpServer.getInstance().setP8Tool(p8Tool);
				TcpServer.getInstance().start();
			}
		});
	}

	JTextField getIpTextField() {
		return ipTextField;
	}


	boolean validateIp() {
		return P8Config.validateIp(ipTextField.getText());
	}
	
	boolean validatePw() {
		return P8Config.validatePw(pwTextField.getText());
	}


	JTextField getPwTextField() {
		return pwTextField;
	}

	void setStatus(P8Status status) {
		controlPanel.setStatus(status);
	}
	
	void resetConfig() {
		confPanel.resetConfig();
	}
}